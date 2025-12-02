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

  def isRepeatedAtLeastTwice(id: String): Boolean = {
    (1 to id.length / 2)
      .to(LazyList)
      .map { i =>
        val init = id.take(i)
        val tail = id.drop(i)
        val sliding = tail.sliding(i, i)
        val allSame = sliding.forall(_ == init)
        allSame
      }
      .find(_ == true)
      .headOption
      .getOrElse(false)
  }

}
