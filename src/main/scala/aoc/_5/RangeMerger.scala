package aoc._5

import aoc._5.InclusiveRangeLong.{Merged, NotMerged}

import scala.annotation.tailrec

object RangeMerger {

  def merge(ranges: Vector[InclusiveRangeLong]): Vector[InclusiveRangeLong] = {

    @tailrec
    def singleMerge(
        ranges: Vector[InclusiveRangeLong],
        notMerged: Vector[InclusiveRangeLong],
        acc: InclusiveRangeLong
    ): (Vector[InclusiveRangeLong], InclusiveRangeLong) = {

      ranges match {
        case x +: xs =>
          acc.merge(x) match {
            case Merged(merged) => singleMerge(xs, notMerged, merged)
            case InclusiveRangeLong.NotMerged =>
              singleMerge(xs, notMerged :+ x, acc)
          }
        case _ => (notMerged, acc)
      }
    }

    @tailrec
    def innerMerge(
        ranges: Vector[InclusiveRangeLong],
        acc: Vector[InclusiveRangeLong]
    ): Vector[InclusiveRangeLong] = {

      ranges match {
        case x +: xs =>
          val (notMerged, merged) = singleMerge(xs, Vector.empty, x)
          innerMerge(notMerged, acc :+ merged)
        case _ => acc
      }
    }

    @tailrec
    def loop(ranges: Vector[InclusiveRangeLong]): Vector[InclusiveRangeLong] = {
      val newRanges = innerMerge(ranges, Vector.empty)
      if newRanges == ranges then ranges
      else loop(newRanges)
    }

    loop(ranges)
  }

}
