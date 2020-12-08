package org.lomiarz.aoc2020

object Day2 {
  def main(args: Array[String]): Unit = {
    val lines = readLines("day2")
    val regexp = """^(\d+)-(\d+)\s([a-z]):\s(.*)""".r

    val resultPart1 = lines
      .count {
        case regexp(from, to, letter, password) =>
          Range(from.toInt, to.toInt + 1).contains(password.count(_.toString == letter))
      }

    println(resultPart1)

    val resultPart2 = lines
      .count {
        case regexp(index1, index2, letter, password) =>
          password.charAt(index1.toInt - 1).toString == letter ^ password.charAt(index2.toInt - 1).toString == letter
      }

    println(resultPart2)
  }
}
