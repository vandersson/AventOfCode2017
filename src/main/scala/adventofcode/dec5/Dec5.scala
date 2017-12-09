package adventofcode.dec5

import adventofcode.ReadFile

object Part1 {
  var jumpList: Array[Int] = Array()

  def main(args: Array[String]): Unit = {
    jumpList = ReadFile.get_input_lines(getClass(), "input")
      .map(_.toInt)
      .toArray

    println(jump(0, 0))
  }

  def jump(pos: Int, steps: Int): Int =
    if (pos < 0  || jumpList.length <= pos)
      steps
    else {
      val npos = jumpList(pos)
      val offset = if (npos >= 3) -1 else 1
      jumpList(pos) = npos+offset
      jump(pos+npos, steps+1)
    }



}
