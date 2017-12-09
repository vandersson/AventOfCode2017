package adventofcode.dec4

import adventofcode.ReadFile

object Part1 {

  def main(args: Array[String]): Unit = {
    val num = ReadFile.get_input_lines(getClass(), "part1.input")
      .map(line => line.split("\\s+"))
      .filter(Code.policy_1(_))
      .size
    println(num)

  }
}

object Part2 {

  def main(args: Array[String]): Unit = {
    val num = ReadFile.get_input_lines(getClass(), "input")
      .map(line => line.split("\\s+"))
      .filter(Code.policy_1(_))
      .filter(Code.policy_2(_))
      .size
    println(num)
  }
}

object Code {
  def policy_1(line: Array[String]): Boolean =
    line.map(word => line.filter(w => w != word)).filter(nl => nl.size == (line.size - 1)).size == line.size

  def policy_2(line: Array[String]): Boolean = {
    val word_is_anagram: Array[Boolean] = line.zipWithIndex.map {
      case(e, i) => is_anagram_of_any(e, line.slice(0, i) ++ line.slice(i + 1, line.length))
    }
    println(s"valid: ${!word_is_anagram.contains(false)} for line: ${line.mkString(" ")}")
    !word_is_anagram.contains(false)
  }

  def is_anagram_of_any(item: String, rest: Array[String]): Boolean = {
    val chars = item.toCharArray.sorted
    rest.filter(word => word.toCharArray.sorted.deep == chars.deep).size == 0
  }
}