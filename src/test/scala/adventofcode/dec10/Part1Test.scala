import adventofcode.dec10.{Dec10, Part1}
import org.scalatest._

class Part1Test extends FlatSpec {

  "first to elements" should "be 12" in {
    val l = Array.range(0, 5)
    val inst = List(3, 4, 1, 5)
    val hash = Dec10_2.hash(l, inst, 0, 0)
    assert(hash(0)*hash(1) === 12)
  }

}