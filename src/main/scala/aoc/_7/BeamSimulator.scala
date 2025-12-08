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
          case ((indexes, count), (sign, index)) =>
            sign match {
              case '.' => (indexes :+ index, count)
              case '^' =>
                val left =
                  if index - 1 < 0 then Vector()
                  else Vector(index - 1)

                val right =
                  if index + 1 > rowBelow.length - 1 then Vector()
                  else Vector(index + 1)

                (indexes :++ left :++ right, count + 1)
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

  def countQuantumPath(): Long =
    countQuantumPathInternal(board, turn, Map.empty)

  @tailrec
  private def countQuantumPathInternal(
      board: Vector[Vector[Char]],
      turn: Int,
      indexToCountAcc: Map[Long, Long]
  ): Long = {

    if turn > board.length - 2 then indexToCountAcc.values.sum
    else {

      val beamSources = board(turn).zipWithIndex
        .filter { case (sign, index) => sign == '|' | sign == 'S' }
      val rowBelow = board(turn + 1).zipWithIndex
      val beamSourcesIndexes = beamSources.map { case (_, i) => i }
      val belowBeamSources = rowBelow.filter { case (sign, index) =>
        beamSourcesIndexes.contains(index)
      }

      val (indexesForBeam, powerAcc) =
        calculateIndexesWithTracking(
          belowBeamSources,
          indexToCountAcc,
          rowBelow
        )

      val newRow = rowBelow.map { case (sign, index) =>
        if indexesForBeam.contains(index) then '|'
        else sign
      }

      val newBoard =
        if turn == 0 then Vector(board(0), newRow) :++ board.drop(2)
        else board.take(turn + 1) :+ newRow :++ board.drop(turn + 2)

      countQuantumPathInternal(newBoard, turn + 1, powerAcc)
    }
  }

  private def calculateIndexesWithTracking(
      belowBeamSources: Vector[(Char, Int)],
      indexToCountAcc: Map[Long, Long],
      rowBelow: Vector[(Char, Int)]
  ): (Vector[Int], Map[Long, Long]) = {

    belowBeamSources.foldLeft(
      (Vector.empty[Int], Map.empty[Long, Long])
    ) { case ((indexes, quantumCountAcc), (sign, index)) =>
      val power = indexToCountAcc.getOrElse(index, 1L)
      sign match {
        case '.' =>
          (
            indexes :+ index,
            quantumCountAcc.updatedWith(index) {
              case Some(value) => Some(value + power)
              case None        => Some(power)
            }
          )
        case '^' =>
          val left =
            if index - 1 < 0 then None
            else Some((index - 1, power))

          val right =
            if index + 1 > rowBelow.length - 1 then None
            else Some((index + 1, power))

          (left, right) match {
            case (
                  Some((leftIndex, leftPower)),
                  Some((rightIndex, rightPower))
                ) =>
              val newAcc = {
                quantumCountAcc
                  .updatedWith(leftIndex) {
                    case Some(value) => Some(value + leftPower)
                    case None        => Some(leftPower)
                  }
                  .updatedWith(rightIndex) {
                    case Some(value) => Some(value + rightPower)
                    case None        => Some(rightPower)
                  }
              }
              (indexes :+ leftIndex :+ rightIndex, newAcc)
            case (Some((leftIndex, leftPower)), None) =>
              val newAcc =
                quantumCountAcc.updatedWith(leftIndex) {
                  case Some(value) => Some(value + leftPower)
                  case None        => Some(leftPower)
                }
              (indexes :+ leftIndex, newAcc)
            case (
                  None,
                  Some((rightIndex, rightPower))
                ) =>
              val newAcc =
                quantumCountAcc.updatedWith(rightIndex) {
                  case Some(value) => Some(value + rightPower)
                  case None        => Some(rightPower)
                }
              (indexes :+ rightIndex, newAcc)
            case (None, None) => (indexes, quantumCountAcc)
          }
      }
    }
  }
 
  def draw(): String =
    board.map { row =>
      row.mkString + "\n"
    }.mkString

}
