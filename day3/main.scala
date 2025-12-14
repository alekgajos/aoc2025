//> using scala 3.4.1

package day3

import scala.io.Source
import scala.collection.mutable.HashMap

def memoize[I, O](f: I => O): I => O = new HashMap[I, O]() { self =>
  override def apply(key: I) = self.synchronized(getOrElseUpdate(key, f(key)))
}

class Problem(filePath: String) {

  val cache = new HashMap[(Array[Char], Int), Long]

  def findJoltage(input: Array[Char]): Int = {

    val battery = input.map(_.asDigit)

    // import scala.math.Ordering.Implicits
    val first = battery.max
    val first_pos = battery.indexOf(first)

    val left = battery.slice(0, first_pos)
    val right = battery.slice(first_pos + 1, battery.length)

    if right.length == 0 then
      val lmax = left.max
      10 * lmax + first
    else
      val rmax = right.max
      10 * first + rmax
  }

  lazy val findJoltageDynamic: ((Array[Char], Int)) => Long = memoize {
    case (input: Array[Char], k: Int) => {
      if (input.length < k) { 0 }
      else if (k == 1 && input.length == 1) {
        input(0).asDigit.toLong
      } else if (k == 0) {
        0
      } else {

        val range = 0 until (input.length - (k - 1))
        val max_val = input.slice(0, input.length - (k - 1)).max

        range
          .filter(input(_) == max_val)
          .map(x => {
            math.pow(10, k - 1).toLong * input(
              x
            ).asDigit.toLong + findJoltageDynamic(
              input.slice(x + 1, input.length),
              k - 1
            )
          })
          .max
      }

    }
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
    val solutions: Seq[Long] =
      parse().map(findJoltageDynamic(_, 12)).toSeq
    solutions.sum
  }

}

@main def main(input: String) = {

  def process(filePath: String) = {
    println(Problem(filePath).solvePart1())
    println(Problem(filePath).solvePart2())
  }

  process(input)
}
