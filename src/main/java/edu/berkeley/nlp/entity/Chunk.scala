package edu.berkeley.nlp.entity

// Chunks are semi-inclusive intervals.
@SerialVersionUID(1L)
case class Chunk[T](val start: Int,
                    val end: Int,
                    val label: T);

object Chunk {
  def seqify[T](chunk: Chunk[T]): Chunk[Seq[T]] = new Chunk(chunk.start, chunk.end, Seq(chunk.label));
}