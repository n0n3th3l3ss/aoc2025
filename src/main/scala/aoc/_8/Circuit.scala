package aoc._8

case class Node(junctionBox: JunctionBox)

case class Edge(first: Node, second: Node) {
  def toCircuit(): Circuit = Circuit(Vector(first, second))

  override def equals(other: Any): Boolean = {

    other match {
      case other: Edge =>
        (first == other.first && second == other.second)
        || (first == other.second && second == other.first)
      case _ => false
    }
  }
}

case class Circuit(
    nodes: Vector[Node]
) {

  def appended(node: Node): Circuit =
    copy(nodes :+ node)

  def contains(node: Node): Boolean = nodes.contains(node)

  def merge(other: Circuit): Circuit = copy(nodes :++ other.nodes)
}
