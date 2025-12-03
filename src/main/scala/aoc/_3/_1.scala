package aoc._3

import aoc.util.Util
import cats.data.EitherT
import cats.effect.IOApp
import cats.effect.IO

import java.nio.file.Path

object _1 extends IOApp.Simple {

  def run: IO[Unit] = {
    for {
      lines <- Util.readLines(Path.of("src/main/resources/_3"))
      batteryBanks = _3_InputParser.parseLines(lines)
      maxJolts = batteryBanks.map(_.twoBatteriesMaxOutput())
      sum = maxJolts.sum
      _ <- IO.println(sum)
    } yield sum
  }

}
