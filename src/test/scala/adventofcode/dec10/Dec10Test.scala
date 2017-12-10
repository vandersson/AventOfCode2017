import adventofcode.dec10.Dec10._
import org.scalatest._

class Dec10Test extends FlatSpec {

  "PART1: first to elements" should "be 12" in {
    val l = Array.range(0, 5)
    val inst = List(3, 4, 1, 5)
    val h = hash(l, inst, 0, 0)
    assert(h(0)*h(1) === 12)
  }

  "PART2: hash of emtpy" should "be a2582a3a0e66e6e86e3812dcb672a272" in {
    assert(improvedHash("") === "a2582a3a0e66e6e86e3812dcb672a272")
  }

  "Part2: has of AoC 2017" should "be 33efeb34ea91902bb2f59c9920caa6cd" in {
    assert(improvedHash("AoC 2017") === "33efeb34ea91902bb2f59c9920caa6cd")
  }

  "Part2: has of 1,2,3" should "be 3efbe78a8d82f29979031a4aa0b16a9d" in {
    assert(improvedHash("1,2,3") === "3efbe78a8d82f29979031a4aa0b16a9d")
  }

  "Part2: has of 1,2,4" should "be 63960835bcdc130f0b66d7ff4f6a5a8e" in {
    assert(improvedHash("1,2,4") === "63960835bcdc130f0b66d7ff4f6a5a8e")
  }
}