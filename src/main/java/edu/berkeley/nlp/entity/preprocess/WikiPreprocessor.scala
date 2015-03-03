package edu.berkeley.nlp.entity.preprocess

import java.io.File

import edu.berkeley.nlp.PCFGLA.CoarseToFineMaxRuleParser
import edu.berkeley.nlp.entity.lang.ModCollinsHeadFinder
import edu.berkeley.nlp.entity._
import edu.berkeley.nlp.entity.ner.NerSystemLabeled
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.futile.syntax.Tree
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.futile.fig.basic.IOUtils

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.xml._
import scala.concurrent._
import scala.collection.JavaConverters._

import ExecutionContext.Implicits.global

/**
 * Created by matthew on 2/21/15.
 */
object WikiPreprocessor {

  val headFinder = new ModCollinsHeadFinder()

  def processesDocs (inputDir : String, outputDir : String,
                     docReader : WikiDocReader,
                     splitter : SentenceSplitter,
                     parser : CoarseToFineMaxRuleParser,
                     backoffParser : CoarseToFineMaxRuleParser,
                     nerSystem : NerSystemLabeled) = {
    val wikiDocs = new File(inputDir).listFiles.map(file => {
      val input_file = file.getAbsolutePath
      val output_file = outputDir + file.getName
      Future {
        try {
          process(input_file, output_file, docReader, splitter, parser.newInstance, backoffParser.newInstance, nerSystem)
        } catch {
          case e : Exception => {
            Logger.logss("failed file: "+input_file)
            System.err.print(e.toString)
            e.printStackTrace(System.err)
            null
          }
        }
      }
    }).map(f => {
      Await.result(f, duration.Duration.Inf)
      f.value.get.get
    }).filter(_ != null).toList
    GUtil.save(wikiDocs.asInstanceOf[Serializable], outputDir + "wiki-docs.doc.ser.gz")
  }

  def process(inputFile : String, outputFile : String,
              docReader : WikiDocReader,
              splitter : SentenceSplitter,
              parser : CoarseToFineMaxRuleParser,
              backoffParser : CoarseToFineMaxRuleParser,
              nerSystem : NerSystemLabeled) : WikiDoc = {
    val wdoc = mkWikiDoc(inputFile, docReader, splitter, parser, backoffParser, nerSystem)
    val lines = wikiToConllLines(wdoc)
    val wlines = wikiToWikiLines(wdoc)
    //PreprocessingDriver.writeConllLines(wdoc.docID, lines.map(_.toArray).toArray, outputFile)
    writeWikiLines(wdoc.docID, lines, outputFile)
    writeWikiLines(wdoc.docID, wlines, outputFile.replace("raw", "wiki"))
    wdoc
  }

  def writeWikiLines(docID : String, lines : Seq[Seq[String]], outputFile : String) = {
    var writer = IOUtils.openOutHard(outputFile)
    writer.println("#begin document (" + docID + "); part 000")
    lines.foreach(l => {
      l.foreach(writer.println(_))
      writer.println
    })
    writer.close()
  }

  def wikiToConllLines(wdoc : WikiDoc) : Seq[Seq[String]] = {
    val ret = ListBuffer[Seq[String]]()
    //ret.append("#begin document (" + wdoc.docID + "); part " + wdoc.docPartNo)
    for(i <- 0 until wdoc.numSents) {
      val parseBits = PreprocessingDriver.computeParseBits(Reprocessor.convertFromFutileTree(wdoc.trees(i).constTree))
      //val nerBits = PreprocessingDriver.computeNerBits(wdoc.nerChunks(i).toArray)
      val corefBits = computeBits(wdoc.corefChunks(i), wdoc.words(i).size)
      var lines = new ListBuffer[String]()
      // conll: [doc name] [part num] [word num] [word] [pos] [parsebit] [6] [7] [8] [speakers] [nerbit] [corefbit]
      for(j <- 0 until wdoc.words(i).size) {
        lines.append(wdoc.docID + "\t" +
          wdoc.docPartNo + "\t" +
          j + "\t" +
          wdoc.words(i)(j) + "\t" +
          wdoc.pos(i)(j) + "\t" +
          parseBits(j) + "\t" +
          "\t-\t-\t-\t" +
          "-\t" + // speakers
          "*\t" + // nerbit
          corefBits(j) + "\t" // coref bits
        )
      }
      ret.append(lines.toSeq)
    }
    ret.toSeq
  }

  def computeBits[T](items : Seq[Chunk[T]], len : Int) : Array[String] = {
    var ret = Array.fill(len)(List[String]())
    items.foreach(c => {
      if(c.start == c.end -1) {
        ret(c.start) = ret(c.start) :+ ("(" + c.label + ")")
      } else {
        ret(c.start) = ret(c.start) :+ ("(" + c.label)
        ret(c.end - 1) = ret(c.end - 1) :+ (c.label + ")")
      }
    })
    ret.map(i => {if(i.isEmpty) "-" else i.reduce(_+"|"+_)})
  }

  def wikiToWikiLines(wdoc : WikiDoc) : Seq[Seq[String]] = {
    val ret = ListBuffer[Seq[String]]()
    for(i <- 0 until wdoc.numSents) {
      val lines = new ListBuffer[String]()
      for(j <- 0 until wdoc.words(i).size) {
        var s = ""
        wdoc.wikiRefChunks(i).foreach(c => {
          if(c.start == j)
            s = "(" + c.label
        })
        s += "*"
        wdoc.wikiRefChunks(i).foreach(c => {
          if(c.end == j + 1)
            s += ")"
        })
        lines.append(s)
      }
      ret.append(lines.toSeq)
    }
    ret.toSeq
  }


  def mkWikiDoc(inputFile : String,
              docReader : WikiDocReader,
              splitter : SentenceSplitter,
              parser : CoarseToFineMaxRuleParser,
              backoffParser : CoarseToFineMaxRuleParser,
              nerSystem : NerSystemLabeled) : WikiDoc = {

    Logger.logss("starting processing of " + inputFile)
    val referencesFile = inputFile.replace("RawTexts", "Problems")
    val refxml = XML.loadFile(referencesFile)
    val document = scala.io.Source.fromFile(inputFile).mkString.split("\n")
    val refname = (refxml \ "ReferenceFileName")(0).text.trim


    val references = (refxml \ "ReferenceInstance").map(r => (
      (r \ "SurfaceForm")(0).text.trim,
      (r \ "Offset")(0).text.trim.toInt,
      (r \ "Length")(0).text.trim.toInt,
      (r \ "ChosenAnnotation")(0).text.trim,
      (r \ "AnnotatorId")(0).text.trim,
      (r \ "Annotation")(0).text.trim
      ))

    val canonicalizedParagraphs = splitter.formCanonicalizedParagraphs(document, false, false)
    val sentences = splitter.splitSentences(canonicalizedParagraphs)
    val tokens = SentenceSplitter.tokenize(sentences)


    val doclenratio = sentences.map(_.size).sum.toFloat / document.map(_.size + 1).sum
    def refFinder (ref : (String, Int, Int, String, String, String)) : (Int, Chunk[String]) = {
      val d = doclenratio * (ref._2 + ref._3 / 2.0)
      var cnt = 0
      val wrds = ref._1.replace(" ", "")
      def rank_match(i : Int, j : Int) : Double = {
        val res = tokens(i).drop(j).reduce(_+_)
        for(q <- 0 until Math.min(wrds.size, res.size)) {
          if (res(q) != wrds(q))
            return q.toDouble / wrds.size
        }
        1.0
      }
      for(i <- 0 to sentences.size) {
        cnt += sentences(i).size
        if(cnt > d) {
          // assume that the reference is in this sentence
          var ll = cnt - sentences(i).size + d // estimated place in sentence
          var tcnt = 0
          var best_start = 0
          var best_rank = Double.NegativeInfinity

          for(j <- 0 until tokens(i).size) {
            val r = rank_match(i,j) / Math.abs(ll - tcnt) // try and make the item close to where it should be
            if(r > best_rank) {
              best_start = j
              best_rank = r
            }
            tcnt += tokens(i)(j).size
          }
          var len = 0
          var len_cnt = 0
          for(j <- best_start until tokens(i).size; if len_cnt < wrds.size) {
            len_cnt += tokens(i)(j).size
            len += 1
          }
          return (i, new Chunk(best_start, best_start + len, ref._4))
        }
      }
      (-1, null)
    }

    val refplaces = references.map(refFinder)

    val refsorted = refplaces.foldLeft(Map[Int, List[Chunk[String]]]().withDefaultValue(List()))((m, itm) => {
      if(itm._1 != -1) {
        m.updated(itm._1, m(itm._1) :+ itm._2)
      } else
        m
    })

    val parses: Array[Tree[String]] = tokens.map(t => {
      //try {
        Reprocessor.convertToFutileTree(
          PreprocessingDriver.parse(parser, backoffParser, t.toList.asJava))
      /*} catch {
        case e : java.lang.NullPointerException => {
          null;
        }
      }*/
    })

    // ... filter out the ones where the parses don't match, idk how that is going to effect
    val tps = (tokens, parses, 0 until tokens.size).zipped
      .filter((a,b,c) => a.length == b.getYield.size)

    val indexer = new Indexer[String]()

    val pos = tps._2.map(t => { new ArrayBuffer[String] ++ t.getPreTerminalYield.asScala })

    val trees = for(i <- 0 until tps._1.size) yield {
      val childParentMap = DepConstTree.extractDependencyStructure(tps._2(i), headFinder)
      new DepConstTree(tps._2(i), pos(i), tps._1(i), childParentMap)
    }

    val empty = tps._1.map(l => (0 until l.length).map(a=>"-")).toSeq

    val wikiDoc = new WikiDoc(
      docID=inputFile,
      docPartNo=refname.toInt,
      words=tps._1.toSeq.map(_.toSeq),
      pos=pos,
      trees=trees,
      nerChunks=tps._1.map(a=>Seq()), // todo
      corefChunks=tps._3.map(i => {
        refsorted(i).map(c => new Chunk(c.start, c.end, indexer.getIndex(c.label)))
      }),
      speakers=empty, // todo?
      wikiRefChunks=tps._3.map(refsorted(_))
    )

    Logger.logss("done with "+inputFile)

    wikiDoc
  }

}
