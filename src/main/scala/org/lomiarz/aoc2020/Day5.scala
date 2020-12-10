package org.lomiarz.aoc2020

object Day5 {
  def main(args: Array[String]): Unit = {
    val seats = readLines("day5")

    def decrypt(input: String, upperChar: Char, limit: Int): Int =
      input.zipWithIndex.foldLeft(0) {
        case (acc, (char, position)) =>
          val diff = (limit + 1) / (1 << position + 1)
          if (char == upperChar) acc + diff.toInt else acc
      }

    def toId(row: Int, col: Int): Int = row * 8 + col

    val decripted = seats
      .map { seat =>
        val r = """(\w{7})(\w{3})""".r
        seat match {
          case r(row, col) => (decrypt(row, 'B', 127), decrypt(col, 'R', 7))
        }
      }

    val biggest = decripted.map { case (row, col) => toId(row, col) }.max
    println(s"Day 5 part 1 result = $biggest")

    val allSeats = decripted
      .filter { case (row, _) => row > 0 && row < 127 }
      .map { case (row, col) => toId(row, col) }
      .sorted

    val gap = allSeats
      .zip(allSeats.tail)
      .find { case (current, next) => next - current > 1 }

    println(s"Day 5 part 2 result = ${gap.get._1 + 1}")

  }
}
