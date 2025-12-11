package aoc._8

import scala.annotation.tailrec

type Distance = BigDecimal

object DistanceMeasurer {

  private def findDistances(
      box: JunctionBox,
      boxes: Vector[JunctionBox]
  ): Map[JunctionBox, Distance] =
    boxes.foldLeft(Map.empty[JunctionBox, Distance]) { case (map, otherBox) =>
      map + (otherBox -> box.euclidDistance(otherBox))
    }

  private def findAllDistances(
      boxes: Vector[JunctionBox]
  ): Map[JunctionBox, Map[JunctionBox, Distance]] =
    boxes.foldLeft(Map.empty) { case (map, box) =>
      map + (box -> findDistances(box, boxes.filter(_ != box)))
    }

  def connect(
      boxes: Vector[JunctionBox],
      maxConnections: Int
  ): Vector[Circuit] = {
    val boxToDistancesToOtherBoxes = findAllDistances(boxes)

    val sortedByDist = boxToDistancesToOtherBoxes.iterator
      .flatMap { case (from, innerMap) =>
        innerMap.iterator.map { case (to, dist) => (from, to, dist) }
      }
      .toList
      .distinctBy((j1, j2, _) => Set(j1, j2))
      .sortBy((_, _, dist) => dist)

    @tailrec
    def innerConnection(
        boxes: List[(JunctionBox, JunctionBox, Distance)],
        circuits: Vector[Circuit],
        maxConnections: Int
    ): Vector[Circuit] = {
      if maxConnections <= 0 then circuits
      else {
        boxes match {
          case Nil => circuits
          case (from, to, _) :: tail =>
            val nodeFrom = Node(from)
            val nodeTo = Node(to)

            if circuits.exists { circuit =>
                circuit.contains(nodeFrom) && circuit.contains(nodeTo)
              }
            then innerConnection(tail, circuits, maxConnections - 1)
            else {

              val containsNodeFrom = circuits.find { circuit =>
                circuit.contains(nodeFrom)
              }
              val containsNodeTo = circuits.find { circuits =>
                circuits.contains(nodeTo)
              }

              val newCircuits = (containsNodeFrom, containsNodeTo) match {
                case (Some(from), Some(to)) =>
                  val newCircuit = from.merge(to)
                  circuits.filter(_ != from).filter(_ != to) :+ newCircuit
                case (Some(from), None) =>
                  val newCircuit = from.appended(nodeTo)
                  circuits.filter(_ != from) :+ newCircuit
                case (None, Some(to)) =>
                  val newCircuit = to.appended(nodeFrom)
                  circuits.filter(_ != to) :+ newCircuit
                case _ => circuits :+ Circuit(Vector(nodeTo, nodeFrom))
              }
              innerConnection(tail, newCircuits, maxConnections - 1)
            }
        }
      }
    }

    innerConnection(sortedByDist, Vector.empty, maxConnections - 1)
  }

  def connectIntoOneCircuit(
      boxes: Vector[JunctionBox]
  ): Option[(Node, Node)] = {
    val boxToDistancesToOtherBoxes = findAllDistances(boxes)

    val sortedByDist = boxToDistancesToOtherBoxes.iterator
      .flatMap { case (from, innerMap) =>
        innerMap.iterator.map { case (to, dist) => (from, to, dist) }
      }
      .toList
      .distinctBy((j1, j2, _) => Set(j1, j2))
      .sortBy((_, _, dist) => dist)

    @tailrec
    def innerConnectionIntoOneCircuit(
        boxes: List[(JunctionBox, JunctionBox, Distance)],
        circuits: Vector[Circuit],
        lastConnected: Option[(Node, Node)]
    ): Option[(Node, Node)] = {
      boxes match {
        case Nil => lastConnected
        case (from, to, _) :: tail =>
          val nodeFrom = Node(from)
          val nodeTo = Node(to)

          if circuits.exists { circuit =>
              circuit.contains(nodeFrom) && circuit.contains(nodeTo)
            }
          then innerConnectionIntoOneCircuit(tail, circuits, lastConnected)
          else {

            val containsNodeFrom = circuits.find { circuit =>
              circuit.contains(nodeFrom)
            }
            val containsNodeTo = circuits.find { circuits =>
              circuits.contains(nodeTo)
            }

            val (newCircuits, lastConnected) =
              (containsNodeFrom, containsNodeTo) match {
                case (Some(from), Some(to)) =>
                  val newCircuit = from.merge(to)
                  (
                    circuits.filter(_ != from).filter(_ != to) :+ newCircuit,
                    Some((nodeFrom, nodeTo))
                  )
                case (Some(from), None) =>
                  val newCircuit = from.appended(nodeTo)
                  (
                    circuits.filter(_ != from) :+ newCircuit,
                    Some((nodeFrom, nodeTo))
                  )
                case (None, Some(to)) =>
                  val newCircuit = to.appended(nodeFrom)
                  (
                    circuits.filter(_ != to) :+ newCircuit,
                    Some((nodeFrom, nodeTo))
                  )
                case _ =>
                  (
                    circuits :+ Circuit(Vector(nodeTo, nodeFrom)),
                    Some((nodeFrom, nodeTo))
                  )
              }
            innerConnectionIntoOneCircuit(
              tail,
              newCircuits,
              lastConnected
            )
          }
      }
    }

    innerConnectionIntoOneCircuit(
      sortedByDist,
      Vector.empty,
      None
    )
  }
}
