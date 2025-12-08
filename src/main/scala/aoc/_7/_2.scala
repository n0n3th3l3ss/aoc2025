package aoc._7

import aoc.util.Util
import cats.effect.{IO, IOApp}

import java.nio.file.Path

object _2 extends IOApp.Simple {

  def run: IO[Unit] = {
    for {
      lines <- Util.readLines(Path.of("src/main/resources/_7"))
      board = _7_InputParser.parseLines(lines)
      beamSimulator = BeamSimulator(board, 0, 0)
      _ <- IO.println(beamSimulator.countQuantumPath())
    } yield ()
  }

}
