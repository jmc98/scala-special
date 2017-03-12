package temp

import scala.io.Source

/**
  * Created by Michael on 08/10/2016.
  */
object LineCounter {
  def main( args: Array[String] ): Unit = {

    val lines = Source.fromURL("http://www.textfiles.com/ufo/3-19disc.txt").getLines().toList

    val maxLineLength = lines.map(_.length).fold(0)(Math.max)
    val widthOfLength = maxLineLength.toString.length

    //noinspection ScalaMalformedFormatString
    println(lines.map( line => s"%${widthOfLength}d | $line".format(line.length)).mkString("\n"))
  }
}
