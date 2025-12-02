package aoc._2

import aoc._2.{IdChecker, _2_InputParser}
import aoc.util.Util
import cats.data.EitherT
import cats.effect.{IO, IOApp}

import java.nio.file.Path

object _2 extends IOApp.Simple {

  def run: IO[Unit] = {
    (for {
      line <- EitherT.fromOptionF(
        Util
          .readLines(Path.of("src/main/resources/_2"))
          .map(_.headOption),
        Exception("First line not found")
      )
      idRanges <- EitherT.fromEither(_2_InputParser.parse(line))
      idSequences = idRanges.flatMap(_.idSequence())
      repeatedTwice = idSequences.filter(IdChecker.isRepeatedAtLeastTwice)
      sum = repeatedTwice.map(_.toLong).sum
    } yield sum).value.flatMap {
      case Left(err) => IO.println(err.getStackTrace.mkString)
      case Right(value) => IO.println(value)
    }
  }
}
