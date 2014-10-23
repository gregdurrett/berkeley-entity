package edu.berkeley.nlp.entity.sem

object GoogleNgramUtils {

  def fastAccessLine(line: String, fieldIdx: Int, gramSize: Int) = {
    if (fieldIdx == 0) {
      fastAccessFirst(line);
    } else if (fieldIdx == gramSize - 1) {
      fastAccessLast(line);
    } else {
      fastAccessLineHelper(line, fieldIdx);
    }
  }
  
  def fastAccessCount(line: String) = fastAccessCountDouble(line);
  
  def fastAccessCountDouble(line: String) = fastAccessCountString(line).toDouble;
  
  def fastAccessCountLong(line: String) = fastAccessCountString(line).toLong;
  
  def fastAccessCountString(line: String) = {
    var firstSpaceIdxFromEnd = line.size - 1;
    while (firstSpaceIdxFromEnd >= 0 && !Character.isWhitespace(line.charAt(firstSpaceIdxFromEnd))) {
      firstSpaceIdxFromEnd -= 1;
    }
    line.slice(firstSpaceIdxFromEnd + 1, line.size)
  }
  
  private def fastAccessLineHelper(line: String, fieldIdx: Int) = {
    var wordIdx = 0;
    var inSpace = false;
    var startIdx = 0;
    var endIdx = 0;
    var i = 0;
    while (i < line.size) {
      if (Character.isWhitespace(line.charAt(i))) {
        if (!inSpace) {
          inSpace = true;
          wordIdx += 1;
          if (wordIdx == fieldIdx + 1) {
            endIdx = i;
          }
        }
      } else {
        if (inSpace) {
          inSpace = false;
          if (wordIdx == fieldIdx) {
            startIdx = i;
          }
        }
      }
      i += 1;
    }
    line.slice(startIdx, endIdx);
  }
  
  private def fastAccessFirst(line: String) = {
    var firstSpaceIdx = 0;
    while (firstSpaceIdx < line.size && !Character.isWhitespace(line.charAt(firstSpaceIdx))) {
      firstSpaceIdx += 1;
    }
    line.slice(0, firstSpaceIdx);
  }
  
  private def fastAccessLast(line: String) = {
    // Go past space
    var firstSpaceIdxFromEnd = line.size - 1;
    var count = 0;
    while (firstSpaceIdxFromEnd >= 0 && !Character.isWhitespace(line.charAt(firstSpaceIdxFromEnd))) {
      firstSpaceIdxFromEnd -= 1;
    }
    var endOfWordIdx = firstSpaceIdxFromEnd - 1;
    while (endOfWordIdx >= 0 && Character.isWhitespace(line.charAt(endOfWordIdx))) {
      endOfWordIdx -= 1;
    }
    var beginningOfWordIdx = endOfWordIdx - 1;
    while (beginningOfWordIdx >= 0 && !Character.isWhitespace(line.charAt(beginningOfWordIdx))) {
      beginningOfWordIdx -= 1;
    }
    line.slice(beginningOfWordIdx + 1, endOfWordIdx + 1);
  }
  
  def main(args: Array[String]) {
    val line = "! &quot; </S> 1952";
    println(fastAccessLine(line, 0, 3));
    println(fastAccessLine(line, 1, 3));
    println(fastAccessLine(line, 2, 3));
    println(fastAccessCount(line));
  }
}
