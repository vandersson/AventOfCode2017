package adventofcode.dec10

import java.nio.charset.StandardCharsets

import adventofcode.dec10.Dec10._

object Part1 {
  def main(args: Array[String]): Unit = {
    val instructions = List(46, 41, 212, 83, 1, 255, 157, 65, 139, 52, 39, 254, 2, 86, 0, 204)
    val result = hash(Array.range(0, 256), instructions, 0, 0)
    println(result(0) * result(1))
  }
}

object Part2 {

  def main(args: Array[String]): Unit = {
    println(improvedHash("46,41,212,83,1,255,157,65,139,52,39,254,2,86,0,204"))
  }
}

object Dec10 {

  def improvedHash(s: String): String = {
    val input = createInput(s)
    val instructions = rounds(input, 64)
    val sparseHash = hash(Array.range(0, 256), instructions.toList, 0, 0)
    toHex(denseHash(sparseHash))
  }

  def toHex(a: Array[Int]): String = a.map(intToHhex(_)).mkString

  def intToHhex(i: Int): String = {
    val hex = Integer.toHexString(i)
    if (hex.size == 1)
      s"0$hex"
    else
      hex
  }

  def denseHash(rem: Array[Int]): Array[Int] =
    if (rem.length == 0)
      Array[Int]()
    else
      rem.splitAt(16) match {
        case (f, s) =>
          Array(f.reduce((a, b) => a ^ b)) ++ denseHash(s)
      }

  def createInput(s: String) =
    s.getBytes(StandardCharsets.US_ASCII).map(_.toInt) ++ Array[Int](17, 31, 73, 47, 23)

  def rounds(l: Array[Int], r: Int): Array[Int] =
    if (r > 1)
      l ++ rounds(l, r - 1)
    else
      l

  def hash(l: Array[Int], instructions: List[Int], i: Int, skip: Int): Array[Int] = instructions match {
    case Nil =>
      l
    case hd :: td => hash(
      mergeSlice(l,
        circSlice(l, i, i + hd).reverse,
        i),
      td, (i + hd + skip) % l.length, skip + 1)
  }

  def mergeSlice(l: Array[Int], slice: Array[Int], start: Int): Array[Int] = {
    for (i <- 0 to slice.length - 1)
      l.update((i + start) % l.length, slice(i))
    l
  }

  def circSlice(l: Array[Int], start: Int, until: Int): Array[Int] =
    if (until > l.size)
      l.slice(start, l.size) ++ l.slice(0, until % l.length)
    else
      l.slice(start, until)
}
