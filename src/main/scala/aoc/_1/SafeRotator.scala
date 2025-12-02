package aoc._1

sealed trait RotateDirections
case object LeftRotate extends RotateDirections
case object RightRotate extends RotateDirections

type RotateCommand = (RotateDirections, Long)

case class SafeRotator(
    min: Long,
    max: Long,
    currentRotation: Long
) {
  val cycle = max - min

  def rotate(command: RotateCommand): SafeRotator = rotate(Seq(command))

  def rotate(command: Seq[RotateCommand]): SafeRotator = {

    val afterRotation = command.foldLeft(currentRotation) {
      case (rotation, (direction, distance)) =>
        direction match {
          case LeftRotate  => leftRotation(rotation, distance)
          case RightRotate => rightRotation(rotation, distance)
        }
    }

    copy(currentRotation = afterRotation)
  }

  private def rightRotation(rotation: Long, distance: Long): Long = {
    if distance > cycle then rightRotation(rotation, distance % cycle)
    if rotation + distance >= max then {
      val realDistance = (rotation + distance) % cycle
      min + realDistance
    } else rotation + distance
  }

  private def leftRotation(rotation: Long, distance: Long): Long = {
    if distance > cycle then leftRotation(rotation, distance % cycle)
    else if rotation - distance < min then {
      val realDistance = (rotation - distance) % cycle
      max + realDistance
    } else rotation - distance
  }

}
