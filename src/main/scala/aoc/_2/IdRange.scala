package aoc._2

case class IdRange(
    start: String,
    end: String
) {

  def idSequence(): Vector[String] =
    (start.toLong to end.toLong)
      .map(_.toString)
      .toVector
}
