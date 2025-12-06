package aoc._6

sealed trait Operation
case object Mul extends Operation
case object Add extends Operation

object Operation {

  def resolveColumnExpression(
      numbers: Vector[Long],
      operation: Operation
  ): Long = {
    operation match {
      case Mul => mul(numbers)
      case Add => add(numbers)
    }
  }

  private def mul(numbers: Vector[Long]) =
    numbers.foldLeft(1L) { case (acc, number) => acc * number }

  private def add(number: Vector[Long]) =
    number.foldLeft(0L) { case (acc, number) => acc + number }

}
