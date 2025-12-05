package aoc._5

object _5_InputParser {

  def parseLines(
      lines: Seq[String]
  ): (Vector[InclusiveRangeLong], Vector[Long]) = {
    val ranges = lines.takeWhile(!_.isBlank)
    val ids = lines.drop(ranges.length + 1).map(_.toLong).toVector

    val rangesVec = ranges
      .map(range =>
        val split = range.split("-")
        InclusiveRangeLong(split(0).toLong, split(1).toLong)
      )
      .toVector

    (rangesVec, ids)
  }

}
