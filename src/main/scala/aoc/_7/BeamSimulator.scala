package aoc._7

import scala.annotation.tailrec

case class BeamSimulator(
    board: Vector[Vector[Char]],
    turn: Int,
    splitCounter: Int
) {

  def simulateOneTurn(): Option[BeamSimulator] =
    propagate(board, turn).map { case (board, splitCount) =>
      BeamSimulator(board, turn + 1, splitCounter + splitCount)
    }

  def simulate(): Option[BeamSimulator] = {

    @tailrec
    def simulateIntern(simulator: BeamSimulator): Option[BeamSimulator] = {
      simulator.simulateOneTurn() match {
        case Some(value) => simulateIntern(value)
        case None        => Some(simulator)
      }
    }

    simulateIntern(this)

  }

  private def propagate(
      board: Vector[Vector[Char]],
      turn: Int
  ): Option[(Vector[Vector[Char]], Int)] = {

    def propagateIntern(): (Vector[Vector[Char]], Int) = {
      val beamSources = board(turn).zipWithIndex
        .filter { case (sign, index) => sign == '|' | sign == 'S' }
      val rowBelow = board(turn + 1).zipWithIndex
      val beamSourcesIndexes = beamSources.map { case (_, i) => i }
      val belowBeamSources = rowBelow.filter { case (sign, index) =>
        beamSourcesIndexes.contains(index)
      }

      val (indexesForBeam, splitCounts) =
        belowBeamSources.foldLeft((Vector.empty[Int], 0)) {
          case ((vec, acc), (sign, index)) =>
            sign match {
              case '.' => (vec :+ index, acc)
              case '^' =>
                val left =
                  if index - 1 < 0 then Vector()
                  else Vector(index - 1)

                val right =
                  if index + 1 > rowBelow.length - 1 then Vector()
                  else Vector(index + 1)

                (vec :++ left :++ right, acc + 1)
            }
        }

      val newRow = rowBelow.map { case (sign, index) =>
        if indexesForBeam.contains(index) then '|'
        else sign
      }

      if turn == 0 then
        (Vector(board(0), newRow) :++ board.drop(2), splitCounts)
      else
        (board.take(turn + 1) :+ newRow :++ board.drop(turn + 2), splitCounts)
    }

    if turn < board.length - 1 then Some(propagateIntern())
    else None

  }

  def draw(): String =
    board.map { row =>
      row.mkString + "\n"
    }.mkString

}
