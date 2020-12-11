package org.lomiarz.aoc2020

object Day9 {
  def main(args: Array[String]): Unit = {
    val input = readLines("day9").map(_.toLong)
    val (preamble, rest) = input.splitAt(25)

    def part1() = {
      rest.zipWithIndex.find {
        case (value, index) =>
          val permutations = preamble ++ (if (index == 0) Seq() else rest.take(index))
          !permutations.combinations(2).toSeq.map(_.sum).exists(_.equals(value))
      }.get
    }

    val (number, index) = part1()
    println(s"Day 9 part 1 result = $number")

    def part2() = {
      val data = preamble ++ rest.take(index - 1)
      val minGroupSize = 3

      val group = minGroupSize
        .until(index)
        .foldLeft((-1, -1)) {
          case (acc, size) =>
            if (acc == (-1, -1)) {
              0.until(input.size - 1).foldLeft((-1, -1)) {
                case (acc, start) =>
                  if (input.slice(start, start + size).sum == number) (start, start + size) else acc
              }
            } else acc
        }

      val contiguousSet = data.slice(group._1, group._2)

      contiguousSet.min + contiguousSet.max
    }

    println(s"Day 9 part 2 result = ${part2()}")
  }
}
