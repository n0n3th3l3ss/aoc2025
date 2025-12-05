package aoc._5

case class InclusiveRangeLong(
    start: Long,
    end: Long
) {
  import InclusiveRangeLong._

  def contains(element: Long): Boolean = {
    if start <= element && end >= element then true
    else false
  }

  def size(): Long =
    end - start + 1L

  def merge(other: InclusiveRangeLong): MergeResult = {
    if start >= other.start && end <= other.end then // other is superset
      Merged(other)
    else if other.start >= start && other.end <= end then // other is subset
      Merged(this)
    else if other.start >= start && other.start <= end then // append
      Merged(InclusiveRangeLong(start, other.end))
    else if other.end >= start && other.end <= end then // prepend
      Merged(InclusiveRangeLong(other.start, end))
    else NotMerged
  }
}

object InclusiveRangeLong {
  sealed trait MergeResult
  final case class Merged(inclusiveRangeLong: InclusiveRangeLong)
      extends MergeResult
  case object NotMerged extends MergeResult
}
