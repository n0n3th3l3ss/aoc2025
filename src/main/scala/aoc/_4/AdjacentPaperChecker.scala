package aoc._4

object AdjacentPaperChecker {

  def markFewerThanAdjacent(
      matrix: Vector[Vector[Char]],
      maxAdjacent: Int
  ): Vector[Vector[Char]] = {
    matrix.zipWithIndex.map { case (line, lineIndex) =>
      line.zipWithIndex.map { case (cell, index) =>
        if cell != '@' then cell
        else {
          val left =
            if index == 0 then None
            else Some(line(index - 1))

          val right =
            if index == line.length - 1 then None
            else Some(line(index + 1))

          val (prevLeft, prev, prevRight) =
            getAdjacent(lineIndex - 1, index, left, right, matrix)

          val (nextLeft, next, nextRight) =
            getAdjacent(lineIndex + 1, index, left, right, matrix)

          val adjacentFields = left
            :: right
            :: prevLeft
            :: prev
            :: prevRight
            :: nextLeft
            :: next
            :: nextRight
            :: Nil

          val countOfPaperAdjacentField =
            adjacentFields.flatMap(_.toList).count(_ == '@')

          if countOfPaperAdjacentField <= maxAdjacent then 'x'
          else cell
        }
      }
    }

  }

  private def getAdjacent(
      checkLineIndex: Int,
      fieldIndex: Int,
      left: Option[Char],
      right: Option[Char],
      matrix: Vector[Vector[Char]]
  ) = {
    if checkLineIndex < 0 then (None, None, None)
    else if checkLineIndex > matrix.length - 1 then (None, None, None)
    else {
      val prevLine = matrix(checkLineIndex)

      val prev = Some(prevLine(fieldIndex))
      val prevLeft = left.map(_ => prevLine(fieldIndex - 1))
      val prevRight = right.map(_ => prevLine(fieldIndex + 1))

      (prevLeft, prev, prevRight)
    }
  }
}
