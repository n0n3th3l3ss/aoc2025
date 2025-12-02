package aoc._1

type DialCount = Long

import cats.implicits._

case class DialCounterSafeRotator(
    safeRotator: SafeRotator
) {

  def rotate(command: RotateCommand): (DialCount, DialCounterSafeRotator) =
    rotate(Seq(command))

  def rotate(
      commands: Seq[RotateCommand]
  ): (DialCount, DialCounterSafeRotator) = {
    commands
      .foldLeft((0L, safeRotator)) { case ((dialCount, rotator), command) =>
        val newSafeRotator = rotator.rotate(command)
        val rotatorDialCount = countDial(command._1, command._2, rotator)

        (rotatorDialCount + dialCount, newSafeRotator)
      }
      .bimap(identity, DialCounterSafeRotator)
  }

  private def countDial(
      direction: RotateDirections,
      distance: Long,
      safeRotator: SafeRotator
  ): Long = {
    direction match {
      case LeftRotate =>
        val rotation = safeRotator.currentRotation - distance

        if rotation < safeRotator.min then {
          if safeRotator.currentRotation == safeRotator.min then
            (Math.abs(rotation) - 1) / safeRotator.cycle
          else (Math.abs(rotation) + safeRotator.cycle - 1) / safeRotator.cycle
        } else 0

      case RightRotate =>
        val rotation = safeRotator.currentRotation + distance

        if rotation > safeRotator.max then {
          if safeRotator.currentRotation == safeRotator.min then
            (rotation - 1) / safeRotator.cycle
          else if safeRotator.rotate(RightRotate, distance).currentRotation == 0
          then { rotation / safeRotator.cycle - 1 }
          else rotation / safeRotator.cycle
        } else 0
    }

  }
}
