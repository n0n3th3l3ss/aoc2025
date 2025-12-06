package aoc._6

object _6_InputParser {

  def parseLines(lines: Seq[String]): Vector[(Vector[Long], Operation)] = {

    val numbers = lines.init
      .map { line => line.split("\\s++").filter(!_.isBlank) }
      .map(_.map(_.toLong).toVector)
      .toVector

    val operations = lines.lastOption
      .map { line => line.split("\\s++").filter(!_.isBlank).toVector }
      .toVector
      .flatten
      .map {
        case "+" => Add
        case "*" => Mul
      }

    numbers.transpose.zip(operations)
  }


}
