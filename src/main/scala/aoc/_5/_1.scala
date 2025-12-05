package aoc._5

import aoc.util.Util
import cats.effect.{IO, IOApp}

import java.nio.file.Path

object _1 extends IOApp.Simple {

  def run: IO[Unit] = {
    for {
      lines <- Util.readLines(Path.of("src/main/resources/_5"))
      (ranges, ids) = _5_InputParser.parseLines(lines)
      ingredientRanges = IngredientRanges(ranges)
      freshIdsCount = ids.count(ingredientRanges.isFresh)
      _ <- IO.println(freshIdsCount)
    } yield ()
  }

}
