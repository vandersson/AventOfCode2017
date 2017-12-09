package adventofcode.dec6

object Dec6 {

  val initialConfig = List(14, 0, 15, 12, 11, 11, 3, 5, 1, 6, 8, 4, 9, 1, 8, 4)
  //val initialConfig = List(0, 2, 7, 0)



  def main(args: Array[String]): Unit = {
    //println(loopUntilSame(initialConfig, List(), 0))
    println(sizeOfLoop(initialConfig, List(), 0))

  }

  def loopUntilSame(banks: List[Int], previousSolutions: List[List[Int]], steps: Int): Int = {
    val i = indexOfMostBlocks(banks)
    val realloc = redistributeBlocks(banks.updated(i, 0), banks(i), (i+1)%banks.size)
    val stepsInc = steps+1
    if (alreadyHappend(realloc, previousSolutions))
      stepsInc
    else
      loopUntilSame(realloc, realloc :: previousSolutions, stepsInc)
  }

  def sizeOfLoop(banks: List[Int], previousSolutions: List[List[Int]], steps: Int): Int = {
    if (alreadyHappend(banks, previousSolutions))
      steps - previousSolutions.reverse.indexOf(banks)
    else {
      val i = indexOfMostBlocks(banks)
      sizeOfLoop(
        redistributeBlocks(banks.updated(i, 0), banks(i), (i + 1) % banks.size),
        banks :: previousSolutions,
        steps+1)
    }


  }



  def alreadyHappend(banks: List[Int], prev: List[List[Int]]): Boolean =
    prev.exists(l => l == banks)


  def redistributeBlocks(banks: List[Int], remaining: Int, distIndex: Int): List[Int] =
    if (remaining > 0)
      redistributeBlocks(banks.updated(distIndex, banks(distIndex) + 1), remaining - 1, (distIndex + 1) % banks.size)
    else
      banks

  def indexOfMostBlocks(banks: List[Int]): Int = indexOfMostBlocks(banks, 0, 0, 0)

  def indexOfMostBlocks(banks: List[Int], currentHigh: Int, currentHighIndex: Int, index: Int): Int = banks match {
    case Nil => currentHighIndex
    case hd :: td if hd > currentHigh => indexOfMostBlocks(td, hd, index, index + 1)
    case hd :: td => indexOfMostBlocks(td, currentHigh, currentHighIndex, index + 1)
  }
}
