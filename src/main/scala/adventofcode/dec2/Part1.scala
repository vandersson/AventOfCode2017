package adventofcode.dec2

import java.nio.file.{Files, Paths}

import adventofcode.dec2.Common.{int_lines}
import adventofcode.ReadFile._


object Part1 {

  def main(args: Array[String]): Unit = {

    val checksum = int_lines(get_input_lines(getClass(), "part1.input"))
      .map(line_checksum(_))
      .reduce((l, r) => l + r)

    println(checksum)
  }

  def line_checksum(line: Array[Int]): Int = line.max - line.min
}

object Part2 {
  def main(args: Array[String]): Unit = {
    val lines = int_lines(get_input_lines(getClass, "part1.input"))
    val sum = lines.map(line => line.map(elem => calc_line(elem, line.filter(_ != elem))).filter(_.isDefined).map(_.get).head).reduce((l, r) => l+r)
    println(sum)
  }

  def calc_line(elem: Int, rest: Array[Int]): Option[Int] = {
    rest.filter(i => elem % i == 0).map(i => elem / i).headOption
  }
}


object Common {

  def int_lines(lines: List[String]): List[Array[Int]] = lines
    .map(line => line.split("\\s+")
      .map(_.toInt))

}