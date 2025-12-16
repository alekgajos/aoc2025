//> using scala 3.4.1

package day4

import scala.io.Source
import scala.collection.mutable.HashMap
import java.lang.Math.sqrt

def memoize[I, O](f: I => O): I => O = new HashMap[I, O]() { self =>
  override def apply(key: I) = self.synchronized(getOrElseUpdate(key, f(key)))
}

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

sealed trait PositionType;
case class Internal() extends PositionType;
case class Edge() extends PositionType;
case class Corner() extends PositionType;

case class Matrix[T](M: List[List[T]]) {

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

  def positionType(x: Int, y: Int): PositionType = {

    val v_edge = (x == 0 || x == width - 1)
    val h_edge = (y == 0 || y == height - 1)

    if (v_edge && h_edge) {
      Corner()
    } else if (v_edge || h_edge) {
      Edge()
    } else {
      Internal()
    }

  }

  def neighbours(i: Int, j: Int) = for {
    ii <- i - 1 to i + 1
    jj <- j - 1 to j + 1
    neighbour <- apply(ii, jj) if ii != i || jj != j
  } yield neighbour

}

class Problem(filePath: String) {

  def parse() = {
    Source
      .fromFile(filePath)
      .getLines()
      .map(_.toCharArray().toList)
      .toList
  }

  def solvePart1() = {

    val M = Matrix(parse())

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

  def solvePart2() = {
    43
  }

}

@main def main(input: String) = {

  def process(filePath: String) = {
    println(Problem(filePath).solvePart1())
    println(Problem(filePath).solvePart2())
  }

  process(input)
}
