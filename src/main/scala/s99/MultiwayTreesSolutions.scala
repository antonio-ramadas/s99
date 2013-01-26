package s99

import Solutions._
import util.parsing.combinator.RegexParsers

trait MultiwayTreesSolutions {

  case class MTree[+T](value: T, children: List[MTree[T]] = List()) {

    override def toString = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"

    def nodeCount: Int = 1 + children.map(_.nodeCount).sum
    def show(implicit ev: T <:< Char): String = value.toChar + children.map(_.show).mkString + "^"
    def internalPathLength: Int = children.map { c => c.nodeCount + c.internalPathLength }.sum
    def postOrder: List[T] = children.flatMap(_.postOrder) :+ value

    def lispyTree: String = children match {
      case Nil => value.toString
      case xs => "(" + value.toString + " " + xs.map(_.lispyTree).mkString(" ") + ")"
    }
  }

  object MTree {
    def fromLispyTree(string: String): MTree[Char] = new RegexParsers {
      val char = "[a-zA-Z]".r
      val leaf = char ^^ { c => MTree(c.head) }
      val node = "(" ~ char ~ rep(tree) ~ ")" ^^ {
        case "(" ~ v ~ children ~ ")" => MTree(v.head, children)
      }
      val tree: Parser[MTree[Char]] = node | leaf
      val result = parse(tree, string)
    }.result.get
  }

  implicit def string2MTree(s: String): MTree[Char] = new RegexParsers {
    val tree: Parser[MTree[Char]] = "[a-zA-Z]".r ~ rep(tree) ~ "^" ^^ {
      case v ~ children ~ "^" => MTree(v.head, children)
    }
    val result = parse(tree, s)
  }.result.get
}