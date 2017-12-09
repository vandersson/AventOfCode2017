package adventofcode.dec7

import adventofcode.ReadFile
import adventofcode.dec7.Dec7._
import adventofcode.dec7.Part1.getClass

object Part1 {

  def main(args: Array[String]): Unit = {
    val lines = ReadFile.get_input_lines(getClass(), "dec7.input")
    val rootName = findRootName(lines.map(parseLine(_)))
    println(rootName)
  }

}

object Part2 {
  def main(args: Array[String]): Unit = {
    val lines = ReadFile.get_input_lines(getClass(), "dec7.input")
    val holdings = lines.map(parseLine(_))
    val rootName = findRootName(holdings)
    buildStruture(rootName, holdingsIndexedOnName(holdings)) match  {
      case Leaf(_) => println("only one element")
      case root: Node => findUnbalancedChild(root.children) match {
        case None => println("root is unbalanced?!?")
        case Some((c, targetW)) => unbalancedCorrectWeight(c, targetW) match {
          case None => "ingen funnet?!?"
          case Some(correctWeight) => println(s"correctWeight: $correctWeight")
        }
      }
    }



  }
}

object Dec7 {

  val leafProgram = """(\w+)\s+\((\d+)\)""".r
  val nodeProgram = """(\w+)\s+\((\d+)\)\s+->\s+([\s,\w]+)""".r

  def parseLine(p: String): Holding =
    p match {
      case leafProgram(name, weight) => Holding(Program(name, weight.toInt), List())
      case nodeProgram(name, weight, others) => Holding(Program(name, weight.toInt), othersToList(others))
    }

  def othersToList(others: String): List[String] = others.split(",").map(_.trim).toList

  def findRootName(programs: List[Holding]): String = {
    val namesWithParent = programs.flatMap(h => h.others)
    programs.filter(p => !namesWithParent.contains(p.program.name)).head.program.name
  }

  def holdingsIndexedOnName(holdings: List[Holding]): Map[String, Holding] =
    holdings.map(h => h.program.name -> h).toMap

  def buildStruture(forName: String, holdings: Map[String,Holding]): Structure = {
    holdings(forName) match {
      case Holding(p, Nil) => Leaf(p)
      case Holding(p, children) => {
        val childStructs = children.map(child => buildStruture(child, holdings))
        Node(p, childStructs, calcWeight(p, childStructs))
      }
    }
  }

  def calcWeight(program: Program, children: List[Structure]): Int = program.weight + childrensWeight(children)

  def unbalancedCorrectWeight(structure: Structure, targetWeigh: Int): Option[Int] = structure match {
    case Leaf(Program(name, weight)) => if (weight == targetWeigh) None else Some(correctWeight(targetWeigh, weight, weight))
    case Node(Program(name, weight), children, nodeWeight) if nodeWeight == targetWeigh => None
    case Node(Program(name, weight), children, nodeWeight) => findUnbalancedChild(children) match {
      case Some((child, childTargetWeight)) => unbalancedCorrectWeight(child, childTargetWeight)
      case None => Some(correctWeight(targetWeigh, nodeWeight, weight))
    }
  }

  def correctWeight(targetWeigth: Int, actualWeight: Int, itemWeight: Int): Int = itemWeight + (targetWeigth-actualWeight)

  def childrensWeight(children: List[Structure]): Int = children.map(child => structWeight(child)).sum

  def findUnbalancedChild(children: List[Structure]): Option[(Structure, Int)] = {
    val (sameAsFirst, notSameAsFirst) = children.partition(child => structWeight(child) == structWeight(children(0)))
    if (notSameAsFirst.isEmpty)
      None // balanced
    else if (sameAsFirst.size == 1)
      Some(sameAsFirst.head, structWeight(notSameAsFirst.head)) //first is inbalanced
    else
      Some(notSameAsFirst.head, structWeight(sameAsFirst.head)) //since only one can be unbalanced.. it will be the only element in notsameasfirst
  }

  def program(s: Structure) = s match {
    case Leaf(p) => p
    case Node(p, _, _) => p
  }

  def structWeight(struct: Structure) = struct match {
    case Leaf(Program(_, w)) => w
    case Node(_, _, w) => w
  }
}

case class Holding(val program: Program, others: List[String])
case class Program(val name: String, val weight: Int)
sealed trait Structure
case class Leaf(val program: Program) extends Structure
case class Node(val program: Program, val children: List[Structure], val totalNodeWeight: Int) extends Structure
