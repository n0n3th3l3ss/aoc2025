package aoc._1

import aoc.util.Util
import cats.data.EitherT
import cats.effect.{IO, IOApp}

import java.nio.file.Path

object _2 extends IOApp.Simple {
  def run: IO[Unit] = {

    (for {
      lines <- EitherT.liftF(Util.readLines(Path.of("src/main/resources/_1")))
      commands <- EitherT.fromEither(_1_InputParser.parseLines(lines))
      safeRotator = SafeRotator(0, 100, 50)
      dialCounterSafeRotator = DialCounterSafeRotator(safeRotator)
      sumOfZeros = commands
        .foldLeft((0L, dialCounterSafeRotator)) {
          case ((counter, rotator), command) =>
            val (dialCount, newRotator) = rotator.rotate(command)

            if newRotator.safeRotator.currentRotation == 0 then
              (counter + 1 + dialCount, newRotator)
            else (counter + dialCount, newRotator)
        }
        ._1
    } yield sumOfZeros).value.flatMap {
      case Left(err)    => IO.println(err.getStackTrace.mkString)
      case Right(value) => IO.println(value)
    }
  }

}
