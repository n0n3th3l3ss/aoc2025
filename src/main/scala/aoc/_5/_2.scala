package aoc._5

import aoc.util.Util
import cats.effect.{IO, IOApp}

import java.nio.file.Path

object _2 extends IOApp.Simple {

  def run: IO[Unit] = {
    for {
      lines <- Util.readLines(Path.of("src/main/resources/_5"))
      (ranges, _) = _5_InputParser.parseLines(lines)
      mergedRanges = RangeMerger.merge(ranges)
      size = mergedRanges.map(_.size()).sum
      _ <- IO.println(size)
    } yield ()
  }

}
