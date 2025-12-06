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

  def parseWithCaphalopodLogic(
      lines: Seq[String]
  ): Vector[(Vector[Long], Operation)] = {
    val longestLine = lines.map(_.length).max

    val numbers = lines.init
      .map { line =>
        val lineSplitToChars = line.chars().mapToObj(_.toChar).toArray.toVector
        val diff = longestLine - lineSplitToChars.length
        if diff > 0 then lineSplitToChars :++ Vector.fill(diff)(' ')
        else lineSplitToChars
      }
      .transpose
      .map(_.mkString)
      .map(_.trim)
      .map(_.toLongOption)
      .foldLeft(Vector.empty[Vector[Long]]) { case (acc, num) =>
        num match {
          case Some(value) =>
            acc match {
              case xs :+ x => xs :+ (x :+ value)
              case _       => Vector(Vector(value))
            }
          case None => acc :+ Vector.empty
        }
      }

    val operations = lines.lastOption
      .map { line => line.split("\\s++").filter(!_.isBlank).toVector }
      .toVector
      .flatten
      .map {
        case "+" => Add
        case "*" => Mul
      }

    numbers.zip(operations)
  }

}
