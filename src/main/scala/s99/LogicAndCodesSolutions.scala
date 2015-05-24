package s99

import scala.collection.mutable

trait LogicAndCodesSolutions { outer =>

  implicit class ExtendedBoolean(a: Boolean) {
    def and(b: => Boolean): Boolean = outer.and(a, b)
    def or(b: => Boolean): Boolean = outer.or(a, b)
    def nand(b: => Boolean): Boolean = outer.nand(a, b)
    def nor(b: => Boolean): Boolean = outer.nor(a, b)
    def xor(b: => Boolean): Boolean = outer.xor(a, b)
    def impl(b: => Boolean): Boolean = outer.impl(a, b)
    def equ(b: => Boolean): Boolean = outer.equ(a, b)
  }

  // just something to avoid using the built-in operators
  def and(a: Boolean, b: => Boolean): Boolean = if (a) b else false
  def or(a: Boolean, b: => Boolean): Boolean = if (a) true else b
  def not(a: Boolean): Boolean = if (a) false else true

  def nand(a: Boolean, b: => Boolean): Boolean = not(and(a, b))
  def nor(a: Boolean, b: => Boolean): Boolean = not(or(a, b))
  def xor(a: Boolean, b: => Boolean): Boolean = and(or(a, b), not(and(a, b)))
  def impl(a: Boolean, b: => Boolean): Boolean = or(not(a), and(a, b))
  def equ(a: Boolean, b: => Boolean): Boolean = not(xor(a, b))

  def table2(f: (Boolean, Boolean) => Boolean): String = {
    def domain = List((true, true), (true, false), (false, true), (false, false))
    def toString(b: Boolean) = if (b) "true  " else "false "
    ("A     B     result" :: domain.map {
      case (a, b) => List(a, b, f(a, b)).map(toString).reduce(_ + _)
    }).mkString("\n")
  }

  def gray(n: Int): List[String] = {
    def grayAux(left: Int, last: List[String]): List[String] =
      if (left == 0) last
      else grayAux(left - 1, last.map("0" + _) ::: last.reverse.map("1" + _))
    grayAux(n, List(""))
  }

  def huffman(list: List[(String,  Int)]): List[(String, String)] = {
    trait Node extends Ordered[Node] {
      def freq: Int
      def compare(that: Node) = that.freq.compare(freq)
    }
    case class Leaf(symbol: String, freq: Int) extends Node
    case class Internal(left: Node, right: Node) extends Node {
      val freq = left.freq + right.freq
    }

    // a shame there is no immutable priority queue implementation in Scala
    val queue = mutable.PriorityQueue[Node](list.map(Leaf.tupled): _*)
    def buildTree: Node =
      if (queue.length < 2) queue.dequeue()
      else {
        queue += Internal(queue.dequeue(), queue.dequeue())
        buildTree
      }

    def codes(node: Node, prefix: String): List[(String, String)] = node match {
      case Leaf(sym, _) => List((sym, prefix))
      case Internal(left, right) =>
        codes(left, prefix + "0") ::: codes(right, prefix + "1")
    }

    codes(buildTree, "").sortBy(_._1)
  }
}
