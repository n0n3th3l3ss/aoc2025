package aoc._1

import cats.syntax.all._

object _1_InputParser {

  def parseLines(lines: Seq[String]): Either[Exception, Seq[RotateCommand]] =
    lines.traverse(parseLine)

  private def parseLine(line: String): Either[Exception, RotateCommand] = {
    for {
      direction <- direction(line)
      distance <- distance(line)
    } yield (direction, distance)
  }

  private def direction(line: String): Either[Exception, RotateDirections] = {
    for {
      firstChar <- line.headOption.toRight(
        new Exception("First char not found")
      )
      direction <- firstChar match {
        case 'R' => Right(RightRotate)
        case 'L' => Right(LeftRotate)
        case _   => Left(new Exception("Invalid char"))
      }
    } yield direction
  }

  private def distance(line: String): Either[Exception, Long] = {
    for {
      distance <- line.tail.toLongOption.toRight(new Exception("Not a Long"))
    } yield distance
  }
}
