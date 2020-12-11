package org.lomiarz.aoc2020

object Day7 {
  def main(args: Array[String]): Unit = {
    val regexpContain = """(\w+ \w+) bags contain (\d+) (\w+ \w+) bags?(.*)\.""".r
    val regexpDoNotContain = """(\w+ \w+) bags contain no other bags\.""".r
    val bags = readLines("day7").map {
      case regexpContain(color, number, secondColor, rest) =>
        val regexpRest = """(\d+) (\w+ \w+) bags?""".r
        val contain = rest.split(",").map(_.trim).filter(_.nonEmpty).map {
          case regexpRest(n, c) => (c, n.toInt)
        } :+ (secondColor, number.toInt)
        (color, contain.toMap)
      case regexpDoNotContain(color) => (color, Map.empty[String, Int])
    }.toMap

    def part1(color: String = "shiny gold"): Set[String] =
      bags
        .filter { case (_, children) => children.keySet.contains(color) }
        .map { case (color, _) => color }
        .foldLeft(Set.empty[String]) {
          case (acc, color) =>
            acc + color ++ part1(color)
        }

    println(s"Day 7 part 1 result = ${part1().size}")

    def part2(color: String = "shiny gold"): Int = {
      bags(color).foldLeft(1) {
        case (acc, (childColor, numberOfBags)) =>
          acc + numberOfBags * part2(childColor)
      }
    }

    println(s"Day 7 part 2 result = ${part2() - 1}")

  }
}
