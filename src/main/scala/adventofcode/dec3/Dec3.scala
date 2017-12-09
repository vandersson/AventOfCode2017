package adventofcode.dec3

object Dec3 {
  def main(args: Array[String]): Unit = {
    val i = 361527
    val coord = coordinates(i)
    println(s"$i : ${coord}")
    println(Math.abs(coord._1) + Math.abs(coord._2))
  }

  def coordinates(max: Int): (Int, Int) = coordinates(max, 0, 1, 0, 0, 1)
  def coordinates(max: Int, prevSum: Int, sum: Int, ring: Int, middle: Int, sideLen: Int): (Int, Int) =
    if (sum < max) {
      coordinates(max, sum, sum + sides(4,sideLen), ring+1, sideLen, sideLen + 2)
    } else {
      val sub = ring-1
      val x =
        if (max <= prevSum + sideLen-1 || max == sum) {
          ring
        } else if (max < prevSum + sideLen*2 - 2) {
          max-(prevSum+sideLen+(sideLen/2)-1)
        } else if (max <= prevSum + sideLen*3 - 3) {
          -ring
        } else {
          max-(prevSum+3*sideLen-3+(sideLen/2))
        }
      val y =
        if (max == sum) {
          -(sideLen/2)
        } else if (max <= prevSum + sideLen-1) {
          max-(prevSum+(sideLen/2))
        } else if (max <= prevSum + sideLen*2 - 2) {
          ring
        } else if (max <= prevSum + sideLen*3 - 3) {
          (prevSum+2*sideLen-2+(sideLen/2))-max
        } else {
          -ring
        }
      (x, y)
    }

  def sides(num: Int, len: Int) = num*len + num
}
