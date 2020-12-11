package org.lomiarz.aoc2020

object Day8 {
  def main(args: Array[String]): Unit = {
    val r = """(\w+) ([+-]\d+)""".r
    val instructions = readLines("day8")
      .map {
        case r(operation, argument) => (operation, argument.toInt)
      }
      .zipWithIndex
      .map { case ((operation, argument), index) => (index, (operation, argument, 0)) }
      .toMap

    def part1(input: Map[Int, (String, Int, Int)], currentIdx: Int, accumulator: Int): Int = {
      val (operation, argument, nrOfUsages) = input(currentIdx)
      if (nrOfUsages == 0) {
        val accumulatorNextValue = if (operation == "acc") accumulator + argument else accumulator
        val nextInstructionIndex = if (operation == "jmp") currentIdx + argument else currentIdx + 1
        if (input.size == nextInstructionIndex) {
          accumulator
        } else {
          part1(input.updated(currentIdx, (operation, argument, 1)), nextInstructionIndex, accumulatorNextValue)
        }
      } else {
        accumulator
      }
    }

    println(s"Day 8 part 1 result = ${part1(instructions, 0, 0)}")

    def terminationCheck(input: Map[Int, (String, Int, Int)], currentIdx: Int): Boolean = {
      val instruction = input.get(currentIdx)
      if (instruction.isDefined) {
        val (operation, argument, nrOfUsages) = instruction.get
        if (nrOfUsages == 0) {
          val nextInstructionIndex = if (operation == "jmp") currentIdx + argument else currentIdx + 1
          if (input.size == nextInstructionIndex) true
          else
            terminationCheck(input.updated(currentIdx, (operation, argument, 1)), nextInstructionIndex)
        } else {
          false
        }
      } else {
        false
      }
    }

    def part2() = {
      instructions.toSeq
        .find {
          case (index, (operation, argument, nrOfUsages)) =>
            operation match {
              case "jmp" => terminationCheck(instructions.updated(index, ("nop", argument, nrOfUsages)), 0)
              case "nop" => terminationCheck(instructions.updated(index, ("jmp", argument, nrOfUsages)), 0)
              case _ => false
            }
        }
        .map {
          case (index, (operation, argument, nrOfUsages)) =>
            val newOperation = operation match {
              case "jmp" => "nop"
              case "nop" => "jmp"
              case x => x
            }
            instructions.updated(index, (newOperation, argument, nrOfUsages))
        }
        .map(part1(_, 0, 0))
        .get

    }

    println(s"Day 8 part 2 result = ${part2()}")
  }
}
