package aoc._5

case class InclusiveRangeLong(
    start: Long,
    end: Long
) {

  def contains(element: Long): Boolean = {
    if start <= element && end >= element then true
    else false
  }
}
