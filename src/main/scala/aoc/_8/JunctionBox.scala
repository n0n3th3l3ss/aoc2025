package aoc._8

case class JunctionBox(
    x: Long,
    y: Long,
    z: Long
) {

  def euclidDistance(other: JunctionBox): BigDecimal = {
    Math.sqrt(
      Math.pow(other.x - x, 2) +
        Math.pow(other.y - y, 2) +
        Math.pow(other.z - z, 2)
    )
  }

}
