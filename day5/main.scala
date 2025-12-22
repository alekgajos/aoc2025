//> using scala 3.4.1

package day5

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

def combine(seqA: Seq[Range], B: Range): Seq[Range] = {

  if (seqA.isEmpty) {
    Seq(B)
  } else {

    val A = seqA.last

    assert(B.start >= A.start)

    if (B.end <= A.end) {
      seqA
    } else if (B.start > A.end) {
      seqA :+ B
    } else {
      seqA.slice(0, seqA.length - 1) :+ Range(A.start, B.end)
    }
  }
}

case class Range(start: Long, end: Long) {

  def ~(k: Long) = if (k >= this.start && k <= this.end) {
    1
  } else {
    0
  }

  def size() = (this.end - this.start + 1)

}

class Problem(filePath: String) {

  val data = parse()
  val ranges = combineRanges(data(0))

  def parse() = {

    val ranges = ArrayBuffer[Range]()
    val numbers = ArrayBuffer[Long]()

    Source
      .fromFile(filePath)
      .getLines()
      .foreach(line => {
        if (line.contains("-")) {
          val parts = line.split("-")
          ranges += Range(parts(0).toLong, parts(1).toLong)
        } else if (line.length() == 0) {} else {
          numbers += line.toLong
        }
      })

    (ranges, numbers)
  }

  def combineRanges(ranges: ArrayBuffer[Range]) = {

    ranges.sortInPlaceBy(r => (r.start, r.end))
    ranges.foldLeft(Seq())((a, b) => combine(a, b))
  }

  def solvePart1() = {

    val numbers = data(1)

    val fits = for {
      k <- numbers
      r <- ranges
    } yield r ~ k

    fits.sum
  }

  def solvePart2() = {
    ranges.map(_.size()).sum
  }

}

@main def main(input: String) = {

  def process(filePath: String) = {
    println(Problem(filePath).solvePart1())
    println(Problem(filePath).solvePart2())
  }

  process(input)
}
