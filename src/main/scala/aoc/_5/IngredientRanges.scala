package aoc._5

case class IngredientRanges(
    ranges: Vector[InclusiveRangeLong]
) {

  def isFresh(idToCheck: Long): Boolean =
    ranges.exists(_.contains(idToCheck))
}
