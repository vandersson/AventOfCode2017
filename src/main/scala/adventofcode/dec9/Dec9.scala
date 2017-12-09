package adventofcode.dec9

import adventofcode.ReadFile
import adventofcode.dec9.Dec9._

object Part1 {
  def main(args: Array[String]): Unit = {
    val input = readChars("dec9.input")
    val result = procChar(input)
    println(result.groupscore)
    println(s"Still working? - ${result.groupscore == 10820}")
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    val input = readChars("dec9.input")
    val result = procChar(input)
    println(result.garbageChars)
  }
}

object Dec9 {

  def readChars(file: String): List[Char] =
    ReadFile.get_input_lines(getClass(), file).flatMap(_.toCharArray)

  def procChar(stream: List[Char]): GarbageScore = procChar(stream, Start, 0, GarbageScore(0, 0))
  def procChar(stream: List[Char], state: State, currDepth: Int, score: GarbageScore): GarbageScore = stream match {
    case Nil => score
    case '!' :: _ :: td => procChar(td, state, currDepth, score)
    case hd :: td => state match {
      case Start => hd match {
        case '{' => procChar(td, Group, currDepth+1, score)
        case '<' => procChar(td, Garbage, currDepth, score)
        case c => throw new IllegalStateException(s"State $state Illegal char $c")
      }
      case EndThing => hd match {
        case ',' => procChar(td, Start, currDepth, score)
        case '}' => procChar(td, EndThing, currDepth-1, incGroupScore(score, currDepth))
        case c => throw new IllegalStateException(s"State $state Illegal char $c")
       }
      case Garbage => hd match {
        case '>' => procChar(td, EndThing, currDepth, score)
        case _ => procChar(td, Garbage, currDepth, incGarbageCount(score))
      }
      case Group => hd match {
        case '{' => procChar(td, Group, currDepth+1, score)
        case '}' => procChar(td, EndThing, currDepth-1, incGroupScore(score, currDepth))
        case '<' => procChar(td, Garbage, currDepth, score)
        case c => throw new IllegalStateException(s"State $state Illegal char $c")
      }
    }
  }

  def incGarbageCount(score: GarbageScore): GarbageScore =
    score.copy(garbageChars = score.garbageChars+1)

  def incGroupScore(score: GarbageScore, value: Int): GarbageScore =
    score.copy(groupscore = score.groupscore+value)
}

sealed trait State
case object Group extends State
case object Garbage extends State
case object EndThing extends State
case object Start extends State
case class GarbageScore(val groupscore: Int, val garbageChars: Int)



