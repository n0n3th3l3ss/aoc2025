package aoc._2

import cats.syntax.all.*

object _2_InputParser {

  def parse(input: String): Either[Exception, Vector[IdRange]] = {
    val ranges = input.split(",", -1)
    ranges.foldLeft(Vector.empty[Either[Exception, IdRange]]) { case (seq, range) =>
      extractIdRange(seq, range)
    }.traverse(identity)
  }

  private def extractIdRange(seq: Vector[Either[Exception, IdRange]], range: String): Vector[Either[Exception, IdRange]] = {
    range.split("-", -1) match {
      case Array(start, end) => seq :+ Right(IdRange(start, end))
      case _ => seq :+ Left(Exception("Invalid split"))
    }
  }

}
