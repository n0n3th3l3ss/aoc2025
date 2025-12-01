package aoc.util

import cats.effect.IO

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

object Util {

  def readLines(path: Path): IO[Vector[String]] =
    IO.blocking:
      Files.readAllLines(path).asScala.toVector

}
