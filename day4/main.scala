//> using scala 3.4.1

package day4

import scala.io.Source
import scala.collection.mutable.HashMap
import java.lang.Math.sqrt
import scala.runtime.stdLibPatches.language.`3.0`
import scala.collection.mutable.ListBuffer

case class Vec(x: Int, y: Int) {

  def *(other: Vec) = x * other.x + y * other.y
  def *(scale: Int) = Vec(x * scale, y * scale)

  def normalized() = {
    val magnitude = sqrt(x * x + y * y)
    Vec((x / magnitude).toInt, (y / magnitude).toInt)
  }

  def perp(): List[Vec] = (for (
    xx <- -1 to 1;
    yy <- -1 to 1;
    vv = Vec(xx, yy)
    if this * vv == 0 && vv * vv != 0
  ) yield Vec(xx, yy)).toList

}

case class Position(x: Int, y: Int) {
  def +(v: Vec) = Position(x + v.x, y + v.y)
}

case class Matrix[T](M: Array[Array[T]]) {

  lazy val width = M.head.length
  lazy val height = M.length

  def print() = {
    M.foreach(line => {
      println(line.mkString(" "))
    })
  }

  def apply(x: Int, y: Int): Option[T] = {

    if (y >= 0 && y < M.length) {
      if (x >= 0 && x < M(y).length) {
        return Some(M(y)(x))
      }
    }
    None
  }

  def apply(pos: Position): Option[T] = apply(pos.x, pos.y)

  def update(x: Int, y: Int)(value: T) = {
    if (y >= 0 && y < M.length) {
      if (x >= 0 && x < M(y).length) {
        M(x)(y) = value
      }
    }
  }

  def neighbours(i: Int, j: Int) = for {
    ii <- i - 1 to i + 1
    jj <- j - 1 to j + 1
    neighbour <- apply(ii, jj) if ii != i || jj != j
  } yield neighbour

}

class Problem(filePath: String) {

  val M = Matrix(parse())

  def parse() = {
    Source
      .fromFile(filePath)
      .getLines()
      .map(_.toCharArray().toArray)
      .toArray
  }

  def solvePart1() = {

    val nodes = for {
      i <- 0 until M.height
      j <- 0 until M.width
    } yield (i, j)

    nodes.map {
      case (i, j) => {
        M(i, j) match
          case Some('@') => {
            if M.neighbours(i, j).count(_ == '@') < 4 then 1 else 0
          }
          case _ => 0
      }
    }.sum

  }

  def removalRound() = {

    val nodes = for {
      i <- 0 until M.height
      j <- 0 until M.width
    } yield (i, j)

    val removed = nodes.map {
      case (i, j) => {
        M(i, j) match
          case Some('@') => {
            if (M.neighbours(i, j).count(_ == '@') < 4) {
              M.update(j, i)('X')
              1
            } else {
              0
            }
          }
          case _ => 0
      }
    }
    removed.sum
  }

  def solvePart2() = {

    var counts = ListBuffer[Int]()

    var k = removalRound()
    while (k > 0) {
      counts += k
      k = removalRound()
    }
    counts.sum
  }

}

@main def main(input: String) = {

  def process(filePath: String) = {
    println(Problem(filePath).solvePart1())
    println(Problem(filePath).solvePart2())
  }

  process(input)
}
