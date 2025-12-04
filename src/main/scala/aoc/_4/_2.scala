package aoc._4

import aoc.util.Util
import cats.data.EitherT
import cats.effect.{IO, IOApp}

import java.nio.file.Path

object _2 extends IOApp.Simple {

  def run: IO[Unit] = {
    for {
      lines <- Util.readLines(Path.of("src/main/resources/_4"))
      paperGrid = _4_InputParser.parseLines(lines)
      markedPaperGrid = AdjacentPaperChecker.markFewerThanAdjacent(paperGrid, 3)
      sum = markedPaperGrid.flatten.count(_ == 'x')
      _ <- IO.println(sum)
    } yield ()
  }

}
