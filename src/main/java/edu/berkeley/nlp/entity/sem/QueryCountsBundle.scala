package edu.berkeley.nlp.entity.sem
import edu.berkeley.nlp.futile.fig.basic.IOUtils
import edu.berkeley.nlp.futile.util.Counter
import edu.berkeley.nlp.futile.util.Logger
import java.io.File

@SerialVersionUID(1L)
class QueryCountsBundle(val wordCounts: Counter[String],
                        val pairCounts: Counter[(String,String)]) extends Serializable {
}

object QueryCountsBundle {
  
  def createFromFile(path: String) = {
    val wordCounts = new Counter[String];
    val pairCounts = new Counter[(String,String)];
    val cleanedPath = if (path != path.trim) {
      Logger.logss("WARNING: queryCountsFile has spurious spaces for some inexplicable reason; trimming");
      path.trim;
    } else {
      path;
    }
    val lineItr = IOUtils.lineIterator(cleanedPath);
    while (lineItr.hasNext) {
      val line = lineItr.next;
      val fields = line.split("\\s+");
      if (fields.size == 2) {
        wordCounts.incrementCount(fields(0), fields(1).toDouble);
      } else if (fields.size == 3) {
        pairCounts.incrementCount(fields(0) -> fields(1), fields(2).toDouble);
      }
    }
    Logger.logss("Loaded " + pairCounts.size + " query counts from " + path);
    new QueryCountsBundle(wordCounts, pairCounts);
  }
}
