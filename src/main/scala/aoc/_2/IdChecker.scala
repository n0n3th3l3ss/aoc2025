package aoc._2

object IdChecker {

  def isRepeatedTwice(id: String): Boolean = {
    if id.length % 2 == 1 then false
    else {
      val (xs, ds) = id.splitAt(id.length / 2)
      if xs == ds then true
      else false
    }
  }
}
