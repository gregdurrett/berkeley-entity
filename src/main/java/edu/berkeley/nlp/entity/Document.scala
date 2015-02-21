package edu.berkeley.nlp.entity

/**
 * Created by matthew on 2/18/15.
 */
trait Document {
  def docID : String
  def docPartNo : Int
  // arrays of words in each sentence including punc
  def words : Seq[Seq[String]]
  // the gram types of the words
  def pos : Seq[Seq[String]]
  // parse trees of each sentence
  def trees : Seq[DepConstTree]
  // I am guessing the type of the chunk eg: ORG-NAM
  def nerChunks : Seq[Seq[Chunk[String]]]
  // have ranges and identifiers for the unique item that they are referenceing
  // appears [start, end)
  def corefChunks : Seq[Seq[Chunk[Int]]]
  // just use "-" for each in the case that the speaker is unknown
  def speakers : Seq[Seq[String]]

  def numSents : Int = -1

  def uid : (String, Int) = docID -> docPartNo

  def fileName : String

  def printableDocName : String

  def isConversation : Boolean = false

  def getCorrespondingNERChunk (sentIdx : Int, headIdx : Int) : Option[Chunk[String]]
}
