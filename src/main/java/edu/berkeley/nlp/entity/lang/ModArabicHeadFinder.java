package edu.berkeley.nlp.entity.lang;

//import edu.berkeley.nlp.ling.CategoryWordTag;

import java.io.StringReader;
import java.util.HashMap;

import edu.berkeley.nlp.futile.ling.AbstractCollinsHeadFinder;
import edu.berkeley.nlp.futile.syntax.Tree;
import edu.berkeley.nlp.futile.syntax.Trees;
import edu.berkeley.nlp.futile.treebank.TreebankLanguagePack;

/**
 * Implements the HeadFinder found in Michael Collins' 1999 thesis. Except:
 * we've added a head rule for NX, which returns the leftmost item. No rule for
 * the head of NX is found in any of the versions of Collins' head table that we
 * have (did he perhaps use the NP rules for NX? -- no Bikel, CL, 2005 says it
 * defaults to leftmost). These rules are suitable for the Penn Treebank.
 * <p>
 * May 2004: Added support for AUX and AUXG to the VP rules; these cause no
 * interference in Penn Treebank parsing, but means that these rules also work
 * for the BLLIP corpus (or Charniak parser output in general). Feb 2005: Fixes
 * to coordination reheading so that punctuation cannot become head.
 * <p>
 * September 2013 (Greg): Hacked up this Arabic headfinder using Stanford's rules.
 * In the CoNLL automatic annotation dataset, the only tags that appear are:
 *   CC CD DT IN JJ NN NNP NNPS NNS NOFUNC PRP PUNC RB RP UH VB VBD VBN VBP WP WRB
 * and the only nonterminals that appear are:
 *   ADJP ADVP CONJP FRAG INTJ LST NAC NP PP PRN PRT S SBAR SBARQ SQ UCP VP WHADVP WHNP WHPP X
 *
 * @author Christopher Manning
 * @author Greg Durrett
 */

public class ModArabicHeadFinder extends AbstractCollinsHeadFinder {

  static String[] leftExceptPunct = {"left"};
  static String[] rightExceptPunct = {"right"};
  
	public ModArabicHeadFinder() {
		this(new ArabicTreebankLanguagePack());
	}

//	protected int postOperationFix(int headIdx, List<Tree<String>> daughterTrees) {
//		if (headIdx >= 2) {
//			String prevLab = daughterTrees.get(headIdx - 1).getLabel();
//			if (prevLab.equals("CC")) {
//				int newHeadIdx = headIdx - 2;
//				Tree<String> t = daughterTrees.get(newHeadIdx);
//				while (newHeadIdx >= 0 && t.isPreTerminal()
//						&& tlp.isPunctuationTag(t.getLabel())) {
//					newHeadIdx--;
//				}
//				if (newHeadIdx >= 0) {
//					headIdx = newHeadIdx;
//				}
//			}
//		}
//		return headIdx;
//	}

	@SuppressWarnings("unchecked")
	public ModArabicHeadFinder(TreebankLanguagePack tlp) {
		super(tlp);
    defaultRule = new String[]{ "right" };
    nonTerminalInfo = new HashMap<String,String[][]>();

    nonTerminalInfo.put("ADJP", new String[][]{{"rightdis", "JJ"}, {"right", "ADJP", "NN", "NNP", "NOFUNC", "NNPS", "NNS"}, {"right", "RB", "CD"}, {"right", "DT"}}); // sometimes right, sometimes left headed??
    nonTerminalInfo.put("ADVP", new String[][]{{"left", "WRB", "RB", "ADVP", "WHADVP"}, {"left", "CD", "RP", "NN", "CC", "ADJ", "IN", "NP", "NNP", "NOFUNC"}}); // NNP is a gerund that they called an unknown (=NNP, believe it or not...)
    nonTerminalInfo.put("CONJP", new String[][]{{"right", "IN", "RB", "NN","NNS","NNP", "NNPS"}});
    nonTerminalInfo.put("FRAG", new String[][]{{"left", "NN", "NNPS", "NNP","NNS" }, {"left", "VBP"}});
    nonTerminalInfo.put("INTJ", new String[][]{{"left", "RP", "UH"}});
    nonTerminalInfo.put("LST", new String[][]{{"left"}});
    nonTerminalInfo.put("NAC", new String[][]{{"left", "NP", "SBAR", "PP", "ADJP", "S", "PRT", "UCP"}, {"left", "ADVP"}}); // note: maybe CC, RB should be the heads?
    nonTerminalInfo.put("NP", new String[][]{{"left", "NN", "NNS", "NNP", "NNPS", "NP", "PRP", "WHNP", "QP", "WP", "NOFUNC"}, {"left", "JJ"}, {"right", "CD"}, {"left", "PRP$"}, {"right", "DT"}}); // should the JJ rule be left or right?
    nonTerminalInfo.put("PP", new String[][]{{"left", "IN", "PP", "PRT", "X"}, {"left", "NNP", "RP", "NN"}, {"left", "NP"}}); // NN is for a mistaken "fy", and many wsT
    nonTerminalInfo.put("PRN", new String[][]{{"left", "NP"}}); // don't get PUNC
    nonTerminalInfo.put("PRT", new String[][]{{"left", "RP", "PRT", "IN"}});
    nonTerminalInfo.put("S", new String[][]{{"left", "VP", "S"}, {"right", "PP", "ADVP", "SBAR", "UCP", "ADJP"}}); // really important to put in -PRD sensitivity here!
    nonTerminalInfo.put("SBAR", new String[][]{{"left", "WHNP", "WHADVP", "WRB", "RP", "IN", "SBAR", "CC", "WP", "WHPP", "ADVP", "PRT", "RB", "X"}, {"left", "NN", "NNP", "NNS", "NNPS"}, {"left", "S"}});
    nonTerminalInfo.put("SBARQ", new String[][]{{"left", "WHNP", "WHADVP", "RP", "IN", "SBAR", "CC", "WP", "WHPP", "ADVP", "PRT", "RB", "X"}, {"left", "NN", "NNP", "NNS", "NNPS"}, {"left", "S"}}); // copied from SBAR rule -- look more closely when there's time
    nonTerminalInfo.put("SQ", new String[][]{{"left", "VP", "PP"}}); // to be principled, we need -PRD sensitivity here too.
    nonTerminalInfo.put("UCP", new String[][]{{"left"}});
    nonTerminalInfo.put("VP", new String[][]{{"left", "VBD", "VBN", "VBP", "VBG", "VP", "RB", "X","VB"}, {"left", "IN"}, {"left", "NNP","NOFUNC", "NN"}}); // exclude RP because we don't want negation markers as heads -- no useful information?
    nonTerminalInfo.put("WHADVP", new String[][]{{"left", "WRB", "WP"}, {"right", "CC"}, {"left", "IN"}});
    nonTerminalInfo.put("WHNP", new String[][]{{"right", "WP"}});
    nonTerminalInfo.put("WHPP", new String[][]{{"left",  "IN", "RB"}});
    nonTerminalInfo.put("X", new String[][]{{"left"}});
    nonTerminalInfo.put(tlp.startSymbol(), new String[][]{{"left"}});
    
    /////////////////////////////
    // ORIGINAL STANFORD RULES //
    /////////////////////////////
//    nonTerminalInfo.put("NX", new String[][]{{"left", "DT","DTNN","DTNNS","DTNNP", "DTNNPS", "DTJJ", "DTNOUN_QUANT", "NOUN_QUANT"}});
//    nonTerminalInfo.put("ADJP", new String[][]{{"rightdis", tagSet.adj(), "DTJJ", "ADJ_NUM", "DTADJ_NUM", "JJR", "DTJJR"}, {"right", "ADJP", "VN", tagSet.noun(), "NNP", "NOFUNC", "NO_FUNC", "NNPS", "NNS", "DTNN", "DTNNS","DTNNP","DTNNPS","DTJJ", "DTNOUN_QUANT", "NOUN_QUANT"}, {"right", "RB", "CD","DTRB","DTCD"}, {"right", "DT"}}); // sometimes right, sometimes left headed??
//    nonTerminalInfo.put("ADVP", new String[][]{{"left", "WRB", "RB", "ADVP", "WHADVP","DTRB"}, {"left", "CD", "RP", tagSet.noun(), "CC", tagSet.adj(), "DTJJ", "ADJ_NUM", "DTADJ_NUM", "IN", "NP", "NNP", "NOFUNC","DTRP","DTNN","DTNNP","DTNNPS","DTNNS","DTJJ", "DTNOUN_QUANT", "NOUN_QUANT"}}); // NNP is a gerund that they called an unknown (=NNP, believe it or not...)
//    nonTerminalInfo.put("CONJP", new String[][]{{"right", "IN", "RB", tagSet.noun(),"NNS","NNP", "NNPS", "DTRB", "DTNN", "DTNNS", "DTNNP", "DTNNPS", "DTNOUN_QUANT", "NOUN_QUANT"}});
//    nonTerminalInfo.put("FRAG", new String[][]{{"left", tagSet.noun(), "NNPS", "NNP","NNS", "DTNN", "DTNNS", "DTNNP", "DTNNPS", "DTNOUN_QUANT", "NOUN_QUANT"}, {"left", "VBP"}});
//    nonTerminalInfo.put("INTJ", new String[][]{{"left", "RP", "UH", "DTRP"}});
//    nonTerminalInfo.put("LST", new String[][]{{"left"}});
//    nonTerminalInfo.put("NAC", new String[][]{{"left", "NP", "SBAR", "PP", "MWP","ADJP", "S", "PRT", "UCP"}, {"left", "ADVP"}}); // note: maybe CC, RB should be the heads?
//    nonTerminalInfo.put("NP", new String[][]{{"left", tagSet.noun(), tagSet.detPlusNoun(), "NNS", "NNP", "NNPS", "NP", "PRP", "WHNP", "QP", "WP", "DTNNS", "DTNNPS", "DTNNP", "NOFUNC", "NO_FUNC", "DTNOUN_QUANT", "NOUN_QUANT"}, {"left", tagSet.adj(), "DTJJ", "JJR", "DTJJR", "ADJ_NUM", "DTADJ_NUM"}, {"right", "CD", "DTCD"}, {"left", "PRP$"}, {"right", "DT"}}); // should the JJ rule be left or right?
//    nonTerminalInfo.put("PP", new String[][]{{"left", tagSet.prep(), "PP", "MWP","PRT", "X"}, {"left", "NNP", "RP", tagSet.noun()}, {"left", "NP"}}); // NN is for a mistaken "fy", and many wsT
//    nonTerminalInfo.put("PRN", new String[][]{{"left", "NP"}}); // don't get PUNC
//    nonTerminalInfo.put("PRT", new String[][]{{"left", "RP", "PRT", "IN", "DTRP"}});
//    nonTerminalInfo.put("QP", new String[][]{{"right", "CD", "DTCD", tagSet.noun(), tagSet.adj(), "NNS", "NNP", "NNPS", "DTNN", "DTNNS", "DTNNP", "DTNNPS", "DTJJ", "DTNOUN_QUANT", "NOUN_QUANT"}});
//
//    nonTerminalInfo.put("S", new String[][]{{"left", "VP", "MWV", "S"}, {"right", "PP", "MWP","ADVP", "SBAR", "UCP", "ADJP"}}); // really important to put in -PRD sensitivity here!
//    nonTerminalInfo.put("SQ", new String[][]{{"left", "VP", "MWV", "PP", "MWP"}}); // to be principled, we need -PRD sensitivity here too.
//    nonTerminalInfo.put("SBAR", new String[][]{{"left", "WHNP", "WHADVP", "WRB", "RP", "IN", "SBAR", "CC", "WP", "WHPP", "ADVP", "PRT", "RB", "X", "DTRB", "DTRP"}, {"left", tagSet.noun(), "NNP", "NNS", "NNPS", "DTNN", "DTNNS", "DTNNP", "DTNNPS", "DTNOUN_QUANT", "NOUN_QUANT"}, {"left", "S"}});
//    nonTerminalInfo.put("SBARQ", new String[][]{{"left", "WHNP", "WHADVP", "RP", "IN", "SBAR", "CC", "WP", "WHPP", "ADVP", "PRT", "RB", "X"}, {"left", tagSet.noun(), "NNP", "NNS", "NNPS","DTNN", "DTNNS", "DTNNP", "DTNNPS", "DTNOUN_QUANT", "NOUN_QUANT"}, {"left", "S"}}); // copied from SBAR rule -- look more closely when there's time
//    nonTerminalInfo.put("UCP", new String[][]{{"left"}});
//    nonTerminalInfo.put("VP", new String[][]{{"left", "VBD", "VBN", "VBP", "VBG", "DTVBG", "VN", "DTVN", "VP", "RB", "X","VB"}, {"left", "IN"}, {"left", "NNP","NOFUNC", tagSet.noun(), "DTNN", "DTNNP", "DTNNPS", "DTNNS", "DTNOUN_QUANT", "NOUN_QUANT"}}); // exclude RP because we don't want negation markers as heads -- no useful information?
//
//    nonTerminalInfo.put("MWV", new String[][]{{"left", "VBD", "VBN", "VBP", "VBG", "DTVBG", "VN", "DTVN", "VP", "RB", "X","VB"}, {"left", "IN"}, {"left", "NNP","NOFUNC", tagSet.noun(), "DTNN", "DTNNP", "DTNNPS", "DTNNS", "DTNOUN_QUANT", "NOUN_QUANT"}}); // exclude RP because we don't want negation markers as heads -- no useful information?
//    nonTerminalInfo.put("MWP", new String[][]{{"left", tagSet.prep(), "PP", "MWP","PRT", "X"}, {"left", "NNP", "RP", tagSet.noun()}, {"left", "NP"}}); // NN is for a mistaken "fy", and many wsT
//
//    
//    //also, RB is used as gerunds
//
//    nonTerminalInfo.put("WHADVP", new String[][]{{"left", "WRB", "WP"}, {"right", "CC"}, {"left", "IN"}});
//    nonTerminalInfo.put("WHNP", new String[][]{{"right", "WP"}});
//    nonTerminalInfo.put("WHPP", new String[][]{{"left",  "IN", "RB"}});
//    nonTerminalInfo.put("X", new String[][]{{"left"}});
//
//    //Added by Mona 12/7/04 for the newly created DT nonterm cat
//    nonTerminalInfo.put("DTNN", new String[][]{{"right"}});
//    nonTerminalInfo.put("DTNNS", new String[][]{{"right"}});
//    nonTerminalInfo.put("DTNNP", new String[][]{{"right"}});
//    nonTerminalInfo.put("DTNNPS", new String[][]{{"right"}});
//    nonTerminalInfo.put("DTJJ", new String[][]{{"right"}});
//    nonTerminalInfo.put("DTRP", new String[][]{{"right"}});
//    nonTerminalInfo.put("DTRB", new String[][]{{"right"}});
//    nonTerminalInfo.put("DTCD", new String[][]{{"right"}});
//    nonTerminalInfo.put("DTIN", new String[][]{{"right"}});
//
//    // stand-in dependency:
//    nonTerminalInfo.put("EDITED", new String[][]{{"left"}});
//    nonTerminalInfo.put(tlp.startSymbol(), new String[][]{{"left"}});
//
//    // one stray SINV in the training set...garbage head rule here.
//    nonTerminalInfo.put("SINV", new String[][]{{"left","ADJP","VP"}});
	}

	/**
	 * Go through trees and determine their heads and print them. Just for
	 * debuggin'. <br>
	 * Usage: <code>
	 * java edu.stanford.nlp.trees.CollinsHeadFinder treebankFilePath
	 * </code>
	 *
	 * @param args
	 *          The treebankFilePath
	 */

	public static void main(String[] args) {
		Trees.PennTreeReader reader = new Trees.PennTreeReader(new StringReader("((S (NP (DT the) (JJ quick) (JJ (AA (BB (CC brown)))) (NN fox)) (VP (VBD jumped) (PP (IN over) (NP (DT the) (JJ lazy) (NN dog)))) (. .)))"));
		Tree<String> tree = reader.next();
		System.out.println("tree "+tree);

		ModArabicHeadFinder headFinder = new ModArabicHeadFinder();
		while (!tree.isLeaf()) {
			Tree<String> head = headFinder.determineHead(tree);
			System.out.println("head "+head);
			tree=head;
		}
	}

	private static final long serialVersionUID = -8747319554557223437L;

}
