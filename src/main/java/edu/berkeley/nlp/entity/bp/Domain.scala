package edu.berkeley.nlp.entity.bp

case class Domain[T](val entries: Array[T]) {
  def size = entries.size
  
  def indexOf(entry: T) = entries.indexOf(entry);
  
  def value(idx: Int): T = entries(idx);
  
  override def toString() = entries.foldLeft("")((str, entry) => str + entry + " ").dropRight(1);
}
