package aoc._7

import scala.jdk.CollectionConverters._

object _7_InputParser {

  def parseLines(lines: Seq[String]): Vector[Vector[Char]] =
    lines
      .map(line =>
        line.chars().mapToObj[Char](_.toChar).toList.asScala.toVector
      )
      .toVector

}
