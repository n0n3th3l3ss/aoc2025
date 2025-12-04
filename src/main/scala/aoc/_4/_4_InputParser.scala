package aoc._4
import scala.jdk.CollectionConverters._

object _4_InputParser {

  def parseLines(lines: Seq[String]): Vector[Vector[Char]] = {
    lines.map { line =>
      line.chars().mapToObj(_.toChar).toList.asScala.toVector
    }.toVector

  }

}
