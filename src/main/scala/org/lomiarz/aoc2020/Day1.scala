package org.lomiarz.aoc2020

object Day1 {
  def main(args: Array[String]): Unit = {
    val lines = readLines("day1").map(_.toInt)

    val allTwos = for {
      x <- lines
      y <- lines
    } yield (x, y)

    allTwos
      .find { case (x, y) => x + y == 2020 }
      .map { case (x, y) => x * y }
      .foreach(println)

    val allThrees = for {
      x <- lines
      y <- lines
      z <- lines
    } yield (x, y, z)

    allThrees
      .find { case (x, y, z) => x + y + z == 2020 }
      .map { case (x, y, z) => x * y * z }
      .foreach(println)
  }
}
