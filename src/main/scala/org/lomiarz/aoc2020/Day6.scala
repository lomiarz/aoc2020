package org.lomiarz.aoc2020

object Day6 {
  def main(args: Array[String]): Unit = {
    val groupsAnswers = readLines("day6")
      .foldLeft(Seq.empty[String]) {
        case (acc, line) =>
          if (line.nonEmpty) {
            val last = s"${acc.lastOption.getOrElse("")}\n$line".trim
            acc.dropRight(1) :+ last
          } else {
            acc :+ ""
          }
      }
      .map(_.split("\n"))
      .map(_.map(_.toCharArray.toSet))

    val part1 = groupsAnswers
      .map(_.reduce(_.union(_)))
      .map(_.size)
      .sum

    println(s"Day 6 part 1 result = $part1")

    val part2 = groupsAnswers
      .map(_.reduce(_.intersect(_)))
      .map(_.size)
      .sum

    println(s"Day 6 part 2 result = $part2")
  }
}
