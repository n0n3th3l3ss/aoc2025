package aoc._3

import aoc.util.Util
import cats.data.EitherT
import cats.effect.{IO, IOApp}

import java.nio.file.Path

object _2 extends IOApp.Simple {

  def run: IO[Unit] = {
    for {
      lines <- Util.readLines(Path.of("src/main/resources/_3"))
      batteryBanks = _3_InputParser.parseLines(lines)
      maxJolts = batteryBanks.map(_.batteriesMaxOutput(12))
      sum = maxJolts.sum
      _ <- IO.println(sum)
    } yield ()
  }

}
