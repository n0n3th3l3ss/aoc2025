package aoc._8

import scala.jdk.CollectionConverters.*

object _8_InputParser {

  def parseLines(lines: Seq[String]): Vector[JunctionBox] =
    lines.map { line =>
      line.split(",") match {
        case Array(x, y, z) => JunctionBox(x.toLong, y.toLong, z.toLong)
      }
    }.toVector

}
