package aoc._3

import scala.annotation.tailrec

case class BatteryBank(
    bank: String
) {

  def twoBatteriesMaxOutput(): Long = {
    val (firstDigit, indexOfFirstDigit) = biggestDigit(bank.init)

    val tail = bank.drop(indexOfFirstDigit + 1)
    val (secondDigit, _) = biggestDigit(tail)

    (firstDigit.toString + secondDigit.toString).toLong
  }

  private def biggestDigit(input: String) = {
    input
      .map(_.toString)
      .map(_.toLong)
      .zipWithIndex
      .foldLeft((0L, 0)) { case ((max, indexOfMax), (number, index)) =>
        if number > max then (number, index)
        else (max, indexOfMax)
      }
  }

  def batteriesMaxOutput(batteryCount: Int): Long = {

    @tailrec
    def foo(acc: String, aBank: String, stopCon: Int): String = {
      if stopCon == 0 then acc
      else {
        val (digit, indexOfDigit) = biggestDigit(aBank.dropRight(stopCon - 1))
        val rest = aBank.drop(indexOfDigit + 1)
        val newAcc = acc + digit.toString
        foo(newAcc, rest, stopCon - 1)
      }
    }

    foo("", bank, batteryCount).toLong
  }

}
