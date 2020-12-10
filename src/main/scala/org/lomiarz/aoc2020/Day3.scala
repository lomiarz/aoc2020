package org.lomiarz.aoc2020

object Day3 {
  def main(args: Array[String]): Unit = {
    val grid = readLines("day3").toVector.map(_.split("").toVector)
    val gridWidth = grid(0).length
    val gridHeight = grid.length

    def calculateNumberOfTrees(slope: (Int, Int)): Long =
      0.until(gridHeight)
        .by(slope._2)
        .foldLeft((0, Seq.empty[String])) {
          case ((currentX, acc), y) =>
            val nextX = (currentX + slope._1) % gridWidth
            (nextX, acc :+ grid(y)(currentX))
        }
        ._2
        .count(_.equals("#"))

    def part1 = println(s"Day3 part1 result: ${calculateNumberOfTrees((3, 1))}")
    def part2 = {
      val result = Seq((1, 1), (3, 1), (5, 1), (7, 1), (1, 2)).map(calculateNumberOfTrees).product
      println(s"Day3 part2 result: $result")
    }

    part1
    part2
  }
}
