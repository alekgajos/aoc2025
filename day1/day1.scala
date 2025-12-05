//> using scala 3.4.1

import scala.util.matching.Regex

import scala.io.Source

sealed trait Move
case class LeftMove(steps: Int) extends Move
case class RightMove(steps: Int) extends Move


class State(i: Int, final_zeroes: Int, zero_passes: Int) {
  def +(x: Move) = x match
    case LeftMove(all_steps) => {
      
      var full_turns = (all_steps / 100).toInt
      val steps = all_steps % 100
      
      val new_i = (i - steps) % 100
      val ret = if (new_i < 0) {
        if(i > 0){
          full_turns += 1
        }
        new_i + 100
      } else {
        new_i
      }

      if (ret == 0) {
        State(ret, final_zeroes + 1, zero_passes + full_turns + 1)
      } else {
        State(ret, final_zeroes, zero_passes + full_turns)
      }

    }
    case RightMove(all_steps) => {
      val ret = (i + all_steps) % 100

      var full_turns = (all_steps / 100).toInt
      val steps = all_steps % 100

      if ( i + steps > 100) {
        full_turns += 1
      }

      if (ret == 0) {
        State(ret, final_zeroes + 1, zero_passes + full_turns + 1)
      } else {
        State(ret, final_zeroes, zero_passes + full_turns)
      }
    }

  override def toString(): String = s"Final zeroes: $final_zeroes; zero passes: $zero_passes"

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
          case _               => throw RuntimeException(s"Failed to parse input line $line.")
      )
  }

  def solvePart1() = {
    val moves = parse()
    val state = State(50, 0, 0)
    moves.foldLeft(state)(_ + _)
  }

}

object Day1 extends App {

  val testFile = "test.txt"
  val inputFile = "input.txt"

  def process(filePath: String) = {
    println(Problem(filePath).solvePart1())
  }

  process(testFile)
  process(inputFile)
}
