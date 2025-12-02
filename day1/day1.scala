//> using scala 3.4.1

import scala.util.matching.Regex

import scala.io.Source

sealed trait Move
case class LeftMove(steps: Int) extends Move
case class RightMove(steps: Int) extends Move

object State {
  var zero_count: Int = 0
  var zero_passes: Int = 0
}

class State(i: Int) {
  def +(x: Move) = x match
    case LeftMove(steps) => {
      val new_i = (i - steps) % 100
      val ret = if (new_i < 0) {
        State.zero_passes += (new_i / (-100)).toInt + 1
        new_i + 100
      } else {
        new_i
      }


      if (ret == 0) {
        State.zero_count += 1
      }
      println(ret)
      State(ret)

    }
    case RightMove(steps) => {
      val ret = (i + steps) % 100

      State.zero_passes += ((i+steps) / 100).toInt

      if (ret == 0) {
        State.zero_count += 1
      }
      println(ret)
      State(ret)
    }

  override def toString(): String = s"zeroes: ${State.zero_count}; passes: ${State.zero_passes}"

}

class Problem(filePath: String) {

  def parse() = {
    val Lpattern = """L(\d+)""".r
    val Rpattern = """R(\d+)""".r
    Source
      .fromFile(filePath)
      .getLines()
      .map(line =>
        line match
          case Lpattern(steps) => LeftMove(steps.toInt)
          case Rpattern(steps) => RightMove(steps.toInt)
          case _               => throw RuntimeException("aaaa")
      )
  }

  def solvePart1() = {
    val moves = parse()
    val state = State(50)
    val final_state = moves.foldLeft(state)(_ + _)
    final_state
  }

  var fastForwarded: Boolean = false
  val nrounds: Long = 1000000000

}

object Day1 extends App {

  val testFile = "test.txt"
  val inputFile = "input.txt"

  def process(filePath: String) = {
    println(Problem(filePath).solvePart1())
    // println(Problem(filePath).solvePart2())
  }

  // process(testFile)
  process(inputFile)
}
