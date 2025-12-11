package aoc._8

import aoc.util.Util
import cats.data.OptionT
import cats.effect.{IO, IOApp}

import java.nio.file.Path

object _2 extends IOApp.Simple {

  def run: IO[Unit] = {
    for {
      lines <- Util.readLines(Path.of("src/main/resources/_8"))
      junctionBoxes = _8_InputParser.parseLines(lines)
      mul = DistanceMeasurer
        .connectIntoOneCircuit(junctionBoxes)
        .map { case (n1, n2) => n1.junctionBox.x * n2.junctionBox.x }
      _ <- IO.println(mul)
    } yield ()
  }

}
