package aoc._1

import aoc.util.Util
import cats.data.EitherT
import cats.effect.{IO, IOApp}

import java.nio.file.Path

object _1 extends IOApp.Simple {
  def run: IO[Unit] = {

    (for {
      lines <- EitherT.liftF(Util.readLines(Path.of("src/main/resources/_1")))
      commands <- EitherT.fromEither(_1_InputParser.parseLines(lines))
      safeRotator = SafeRotator(0, 100, 50)
      sumOfZeros = commands
        .foldLeft((safeRotator, 0)) { case ((rotator, counter), command) =>
          val newRotator = rotator.rotate(command)

          if newRotator.currentRotation == 0 then (newRotator, counter + 1)
          else (newRotator, counter)
        }
        ._2
    } yield sumOfZeros).value.flatMap {
      case Left(err)    => IO.println(err.getStackTrace.mkString)
      case Right(value) => IO.println(value)
    }
  }

}
