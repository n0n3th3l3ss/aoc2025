package aoc._6

import aoc.util.Util
import cats.effect.{IO, IOApp}

import java.nio.file.Path

object _1 extends IOApp.Simple {

  def run: IO[Unit] = {
    for {
      lines <- Util.readLines(Path.of("src/main/resources/_6"))
      exprs = _6_InputParser.parseLines(lines)
      results = exprs.map(Operation.resolveColumnExpression)
      sum = results.sum
      _ <- IO.println(sum)
    } yield ()
  }

}
