package org.lomiarz.aoc2020

import scala.util.Try

object Day4 {
  def main(args: Array[String]): Unit = {
    def toPasswordFields(input: String): Map[String, String] = {
      input
        .split(" ")
        .map(_.trim)
        .map(_.split(":", 2).toList)
        .map { case name :: value :: Nil => (name, value) }
        .toMap
    }

    val passports = readLines("day4")
      .foldLeft(Seq.empty[String]) {
        case (acc, line) =>
          if (line.nonEmpty) {
            val last = s"${acc.lastOption.getOrElse("")} $line".trim
            acc.dropRight(1) :+ last
          } else {
            acc :+ ""
          }
      }
      .map(toPasswordFields)

    val passwordFieldValidationRules = Map[String, (Boolean, String => Boolean)](
      "byr" -> (true, (input: String) => Try(input.toInt).map(1920.to(2002).contains(_)).getOrElse(false)),
      "iyr" -> (true, (input: String) => Try(input.toInt).map(2010.to(2020).contains(_)).getOrElse(false)),
      "eyr" -> (true, (input: String) => Try(input.toInt).map(2020.to(2030).contains(_)).getOrElse(false)),
      "hgt" -> (true, (input: String) => {
        val in = """(\d+)in""".r
        val cm = """(\d+)cm""".r
        input match {
          case cm(value) => 150.to(193).contains(value.toInt)
          case in(value) => 59.to(76).contains(value.toInt)
          case _ => false
        }
      }),
      "hcl" -> (true, (input: String) => """#[a-f0-9]{6}""".r.matches(input)),
      "ecl" -> (true, (input: String) => Seq("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(input)),
      "pid" -> (true, (input: String) => """[0-9]{9}""".r.matches(input)),
      "cid" -> (false, (_: String) => true)
    )

    val requiredFields = passwordFieldValidationRules.filter { case (_, (required, _)) => required }.keySet
    println(s"Part 1 result = ${passports.count(passport => requiredFields.subsetOf(passport.keySet))}")

    def isValid(passport: Map[String, String]): Boolean =
      passwordFieldValidationRules.forall {
        case (name, (isRequired, validationFunction)) =>
          (isRequired, passport.get(name)) match {
            case (true, None) => false
            case (true, Some(value)) => validationFunction(value)
            case (false, _) => true
          }
      }

    println(s"Part 2 result = ${passports.count(isValid)}")
  }
}
