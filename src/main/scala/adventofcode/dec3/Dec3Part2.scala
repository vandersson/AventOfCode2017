package adventofcode.dec3

object Dec3Part2 {

  type Matrix = Array[Array[Int]]

  def main(args: Array[String]) = {
    val input = 361527
    val w = 100
    val matrix = Array.ofDim[Int](w, w)
    val center = w / 2
    val origo = Position(center, center)
    matrix(center)(center) = 1
    val e = traverse(matrix, origo, Move(Right, 0, 1, 1), modifier(input))

    println(e.v)

  }

  def modifier(endVal: Int)(matrix: Matrix, p: Position): Condition = {
    val neighbor_sum = matrix(p.x-1)(p.y) + matrix(p.x+1)(p.y) + matrix(p.x)(p.y-1) + matrix(p.x)(p.y+1) +
      matrix(p.x+1)(p.y+1) + matrix(p.x+1)(p.y-1) + matrix(p.x-1)(p.y+1) + matrix(p.x-1)(p.y-1)
    matrix(p.x)(p.y) = neighbor_sum
    if (neighbor_sum > endVal)
      ReturnVal(neighbor_sum, matrix)
    else
      Continue(matrix)
  }
  
  def traverse(matrix: Matrix, pos: Position, move: Move, matrixModifier: (Matrix, Position) => Condition): ReturnVal = {
    val next_pos = next_position(pos, move.direction)
    matrixModifier(matrix, next_pos) match {
      case r: ReturnVal => r
      case Continue(next_matrix) =>
        traverse(next_matrix, next_pos, next_move(move), matrixModifier)
    }
  }

  def next_move(prevMove: Move): Move = prevMove match {
    case Move(dir, prev, curr, step) if prev == curr && curr == step =>
      Move(dir.next(), curr, curr+1, 1)
    case Move(dir, prev, curr, step) if curr == step =>
      Move(dir.next(), curr, curr, 1)
    case m: Move =>
      m.copy(stepsTaken = m.stepsTaken+1)
  }

  def next_position(pos: Position, direction: Direction): Position =
    direction match {
      case Up => pos.copy(y = pos.y + 1)
      case Down => pos.copy(y = pos.y - 1)
      case Left => pos.copy(x = pos.x - 1)
      case Right => pos.copy(x = pos.x + 1)
    }

}

sealed trait Condition
case class ReturnVal(val v: Int, val matrix: Array[Array[Int]]) extends Condition
case class Continue(val matrix: Array[Array[Int]]) extends Condition

case class Position(val x: Int, val y: Int)

case class Move(val direction: Direction, val prevDist: Int, val currDist: Int, val stepsTaken: Int)

sealed trait Direction {
  def next(): Direction
}

case object Right extends Direction {
  override def next() = Up
}
case object Up extends Direction {
  override def next() = Left
}

case object Left extends Direction {
  override def next() = Down
}

case object Down extends Direction {
  override def next() = Right
}

