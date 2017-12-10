package adventofcode.dec10

import adventofcode.dec10.Dec10.hash

object Part1 {
  def main(args: Array[String]): Unit = {
    val instructions = List(46, 41, 212, 83, 1, 255, 157, 65, 139, 52, 39, 254, 2, 86, 0, 204)
    val result = hash(Array.range(0, 256), instructions, 0, 0)
    println(result(0)*result(1))
  }
}

object Dec10 {
  def hash(l: Array[Int], instructions: List[Int], i: Int, skip: Int): Array[Int] = instructions match {
    case Nil =>
      l
    case hd :: td => hash(
      mergeSlice(l,
        circSlice(l, i, i + hd).reverse,
        i),
      td, (i + hd + skip)%l.length, skip + 1)
  }

  def mergeSlice(l: Array[Int], slice: Array[Int], start: Int): Array[Int] = {
    for (i <- 0 to slice.length-1)
      l.update((i+start)%l.length, slice(i))
    l
  }

  def circSlice(l: Array[Int], start: Int, until: Int): Array[Int] =
    if (until > l.size)
      l.slice(start, l.size) ++ l.slice(0, until % l.length)
    else
      l.slice(start, until)
}
