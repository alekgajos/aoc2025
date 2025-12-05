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

def check(range: (Long, Long)): Long = {

  // println(range)

  var count: Long = 0;

  val len_a = count_digits(range(0))
  val len_b = count_digits(range(1))

  if (len_a == len_b && len_a % 2 == 1) { return 0 }

  var number = range(0)
  var len = count_digits(number)
  if (len % 2 == 1) {
      number = math.pow(10, len).toLong
      len += 1
  }
  var first_half = (number / math.pow(10, len / 2)).toLong
  var whole_number = first_half * math.pow(10, len / 2).toLong + first_half

  while (whole_number <= range(1)) {


    if (whole_number <= range(1) && whole_number >= range(0)) {
      count += whole_number.toLong
      // println(whole_number)
    }

    first_half += 1
    whole_number = first_half * math.pow(10, count_digits(first_half)).toLong + first_half
  }

  count
}

class Problem(filePath: String) {

  def parse() = {

    val ranges: Array[String] =
      Source
        .fromFile(filePath)
        .getLines()
        .map(line => {
          // println(line)
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

    ranges.map(check).sum
  }

}

@main def main(input: String) = {

  def process(filePath: String) = {
    println(Problem(filePath).solvePart1())
  }

  process(input)
  // process(inputFile)
}
