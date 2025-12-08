//> using scala 3.4.1

package day3


import scala.io.Source

class Problem(filePath: String) {

  def findJoltage(input: Array[Char]): Int = {

    val battery = input.map(_.asDigit)

    // import scala.math.Ordering.Implicits
    val first = battery.max
    val first_pos = battery.indexOf(first)

    val left = battery.slice(0, first_pos)
    val right = battery.slice(first_pos+1, battery.length)

    if right.length == 0 then
      val lmax = left.max
      10*lmax + first
    else
      val rmax = right.max
      10*first + rmax
  }

  def parse() = {
    Source
      .fromFile(filePath)
      .getLines()
      .map(line => {
        line.toCharArray
      })
  }

  def solvePart1() = {
    parse().map(findJoltage).sum
  }

  def solvePart2() = {
    ()
  }

}

@main def main(input: String) = {

  def process(filePath: String) = {
    println(Problem(filePath).solvePart1())
    // println(Problem(filePath).solvePart2())
  }

  process(input)
}
