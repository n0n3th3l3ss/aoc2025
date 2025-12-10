package aoc._8

import aoc.util.Util
import cats.data.OptionT
import cats.effect.{IO, IOApp}

import java.nio.file.Path

object _1 extends IOApp.Simple {

  def run: IO[Unit] = {
    for {
      lines <- Util.readLines(Path.of("src/main/resources/_8"))
      junctionBoxes = _8_InputParser.parseLines(lines)
      mul = DistanceMeasurer
        .connect(junctionBoxes, 1000)
        .map(_.nodes.length)
        .sorted(summon[Ordering[Int]].reverse)
        .take(3)
        .product
      _ <- IO.println(mul)
    } yield ()
  }

}
