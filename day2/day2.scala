//> using scala 3.4.1

package day2

import scala.io.Source

def count_digits(x: Long) = {
  var scale: Long = 10
  var digits = 1
  while (x / scale >= 1.0) {
    scale *= 10
    digits += 1
  }
  digits
}

def checkSingleNumber(x: Long, part_two: Boolean): Seq[Long] = {

  val len = count_digits(x)

  val range_to_test =
    if part_two then 1 to len else (len + 1) / 2 to (len + 1) / 2

  range_to_test flatMap (l => {
    if (len % l == 0 && l != len) {
      val pattern = (x % math.pow(10, l)).toLong
      val rep = (len / l).toInt
      var whole_number: Long = 0

      1 to rep foreach { _ =>
        whole_number = whole_number * math.pow(10, l).toLong + pattern
      }
      if (whole_number == x) {
        Some(whole_number)
      } else {
        None
      }
    } else {
      None
    }
  })

}

def check(range: (Long, Long), part_two: Boolean): Long = {

  val numbers = range(0) to range(1) flatMap { (x: Long) =>
    val variants =
      checkSingleNumber(x, part_two).filter(x => x <= range(1) && x >= range(0))
    if variants.length > 0 then Some(x) else None
  }

  numbers.sum
}

class Problem(filePath: String) {

  def parse() = {

    val ranges: Array[String] =
      Source
        .fromFile(filePath)
        .getLines()
        .map(line => {
          line.split(",")
        })
        .next()

    ranges
      .map(s => {
        val parts = s.split("-")
        (parts(0).toLong, parts(1).toLong)
      })
      .toSeq
  }

  def solvePart1() = {
    val ranges = parse()
    ranges.map(check(_, false)).sum
  }

  def solvePart2() = {
    val ranges = parse()
    ranges.map(check(_, true)).sum
  }

}

@main def main(input: String) = {

  def process(filePath: String) = {
    println(Problem(filePath).solvePart1())
    println(Problem(filePath).solvePart2())
  }

  process(input)
}
