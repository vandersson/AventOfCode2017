package adventofcode.dec8


import adventofcode.ReadFile
import adventofcode.dec8.Dec8._

import scala.util.{Failure, Success, Try}

object Part1 {
  def main(args: Array[String]): Unit = {
    val expressions = ReadFile.get_input_lines(getClass(), "dec8.input").map(parseExpr(_))
    println(expressions)
    val regs = evaluate(expressions, Map())
    println(largestRegister(regs))
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    val expressions = ReadFile.get_input_lines(getClass(), "dec8.input").map(parseExpr(_))
    val maxes = evaluateWithRunningMax(expressions, Map(), List())
    val max = maxes.maxBy { case (_, v) => v }
    println(max)
  }
}

object Dec8 {

  type Registers = Map[Register, Value]

  def largestRegister(registers: Registers): (Register, Value) =
    registers.maxBy{ case (r, v) => v }

  def evaluate(instructions: List[Expr], registers: Registers): Registers = instructions match {
    case Nil => registers
    case Expr(reg, op, pred) :: td =>
      if (evalPredicate(registers, pred))
        evaluate(td, registers + (reg -> evalOperation(registers.getOrElse(reg, Value(0)), op)))
      else
        evaluate(td, registers)
  }

  def evaluateWithRunningMax(instructions: List[Expr], registers: Registers, maxes: List[(Register, Value)]): List[(Register, Value)] = instructions match {
    case Nil => maxes
    case Expr(reg, op, pred) :: td =>
      if (evalPredicate(registers, pred)) {
        val updatedRegistes = registers + (reg -> evalOperation(registers.getOrElse(reg, Value(0)), op))
        evaluateWithRunningMax(td, updatedRegistes, largestRegister(updatedRegistes) :: maxes)
      }
      else
        evaluateWithRunningMax(td, registers, maxes)
  }

  def evalOperation(value: Value, op: Operation): Value = op match {
    case Inc(v) => Value(value.amount + v)
    case Dec(v) => Value(value.amount - v)
  }

  def evalPredicate(registers: Registers, predicate: Predicate): Boolean = {
    val first = resolvePointer(registers, predicate.first)
    val second = resolvePointer(registers, predicate.second)
    predicate.comp match {
      case Eq => first == second
      case NEq => first != second
      case Gt => first > second
      case GEqt => first >= second
      case Lt => first < second
      case LEqt => first <= second
    }
  }

  def resolvePointer(registers: Registers, pointer: Pointer): Value = pointer match {
    case v: Value => v
    case r: Register => registers.getOrElse(r, Value(0))
  }

  def parseExpr(expr: String): Expr = {
    val parts = expr.split(" ")
    Expr(Register(parts(0)), parseOp(parts(1), parts(2)), parsePredicate(parts(4), parts(5), parts(6)))
  }

  def parseOp(op: String, value: String) =
    if (op == "inc")
      Inc(value.toInt)
    else
      Dec(value.toInt)

  def parsePredicate(first: String, comp: String, second: String): Predicate =
    Predicate(parsePointer(first), parseComp(comp), parsePointer(second))

  def parsePointer(expr: String) = Try (expr.toInt) match {
    case Success(amount) => Value(amount)
    case Failure(_) => Register(expr)
  }


  def parseComp(comp: String): Comparator =
    if (comp == "!=")
      NEq
    else if (comp == "==")
      Eq
    else if (comp == ">")
      Gt
    else if (comp == ">=")
      GEqt
    else if (comp == "<")
      Lt
    else if (comp == "<=")
      LEqt
    else
      throw new IllegalArgumentException(s"Invalid comparator $comp")

}

case class Expr(val reg: Register, val op: Operation, val predicate: Predicate)

sealed trait Pointer
case class Register(val name: String) extends Pointer
case class Value(val amount: Int) extends Pointer with Ordered[Value] {
  override def compare(that: Value) = amount compare that.amount
}

sealed trait Operation
case class Inc(val amount: Int) extends Operation
case class Dec(val amount: Int) extends Operation

sealed trait Comparator
case object NEq extends Comparator
case object Eq extends Comparator
case object Gt extends Comparator
case object GEqt extends Comparator
case object Lt extends Comparator
case object LEqt extends Comparator

case class Predicate(val first: Pointer, val comp: Comparator, val second: Pointer)
