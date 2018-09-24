package s99

import scala.Ordering.Implicits._
import scala.collection.immutable

trait BinaryTreesSolutions {

  sealed abstract class Tree[+T] {
    def isSymmetric: Boolean

    def isMirrorOf(tree: Tree[_]): Boolean

    def addValue[S >: T : Ordering](s: S): Tree[S]

    def leafCount: Int = ???

    def leafList: List[T] = ???

    def internalList: List[T] = ???

    def atLevel(n: Int): List[T] = ???

    def layoutBinaryTree: Tree[T] = ???

    def layoutBinaryTree2: Tree[T] = ???

    def layoutBinaryTree3: Tree[T] = ???

    def show: String = ???

    def preOrder: List[T] = ???

    def inOrder: List[T] = ???

    def toDotString: String = ???
  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString = s"T($value $left $right)"

    override def isMirrorOf(tree: Tree[_]): Boolean = tree match {
      case Node(_, l, r) => left.isMirrorOf(r) && right.isMirrorOf(l)
      case _ => false
    }

    override def isSymmetric: Boolean = isMirrorOf(this)

    override def addValue[S >: T : Ordering](s: S): Tree[S] = {
      if (s < value) {
        Node(value, left.addValue(s), right)
      } else {
        Node(value, left, right.addValue(s))
      }
    }
  }

  case object End extends Tree {
    override def toString = "."

    override def isSymmetric: Boolean = true

    override def isMirrorOf(tree: Tree[_]): Boolean = tree match {
      case End => true
      case _ => false
    }

    override def addValue[S: Ordering](s: S): Tree[S] = Node(s)
  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }

  object Tree {
    def cBalanced[T](n: Int, e: T): List[Tree[T]] = {
      if (n == 0)
        List(End)
      else if ((n - 1) % 2 == 0) { // Even number of children?
        val child = cBalanced((n - 1) / 2, e)
        for {
          left <- child
          right <- child
        } yield Node(e, left, right)
      } else {
        for {
          small <- cBalanced((n - 1) / 2, e)
          big <- cBalanced((n - 1) / 2 + 1, e)
          (left, right) <- List((small, big), (big, small))
        } yield Node(e, left, right)
      }
    }

    def fromList[T: Ordering](list: List[T]): Tree[T] = list.foldLeft(End: Tree[T])(_.addValue(_))

    def symmetricBalancedTrees[T](n: Int, e: T): List[Tree[T]] = cBalanced(n, e).filter(_.isSymmetric)

    def hbalTrees[T](h: Int, e: T): List[Tree[T]] =
      if (h < 0) Nil
      else if (h == 0) List(End)
      else {
        val talls = hbalTrees(h - 1, e)

        val balanced = for {
          left <- talls
          right <- talls
        } yield Node(e, left, right)

        val unbalanced = for {
          tall <- talls
          short <- hbalTrees(h - 2, e)
          (left, right) <- List((tall, short), (short, tall))
        } yield Node(e, left, right)

        balanced ::: unbalanced
      }

    /*
    // Shortest solution
    (for {
      nNodes <- math.pow(2, h-1).toInt until math.pow(2, h).toInt
    } yield cBalanced(nNodes, e)).toList.flatten
    */

    def minHbalNodes(h: Int): Int =
      if (h <= 0) 0
      else if (h == 1) 1
      else minHbalNodes(h - 1) + minHbalNodes(h - 2) + 1

    /*
    // Shortest solution
    math.pow(2, h-1).toInt
     */

    def maxHbalHeight(n: Int): Int =
      if (n <= 0) 0
      else if (n == 1) 1
      else 1 + maxHbalHeight(n - n/2)

    /*
    // Shortest solution (log2(n) + 1)
    // Scala does not have log2 built-in. So, we use the logarithm base change rule to help us out.
    (math.log(n) / math.log(2)).toInt + 1
     */

    def hbalTreesWithNodes[T](n: Int, e: T): List[Tree[T]] = cBalanced(n, e)

    def completeBinaryTree[T](n: Int, e: T): Tree[T] = ???

    def fromString(string: String): Tree[Char] = ???

    def preInTree[T](pre: List[T], in: List[T]): Tree[T] = ???

    def fromDotString(string: String): Tree[Char] = ???
  }

  class PositionedNode[+T](override val value: T,
                           override val left: Tree[T],
                           override val right: Tree[T],
                           val x: Int,
                           val y: Int) extends Node[T](value, left, right) {
    override def toString = s"T[$x,$y]($value $left $right)"
  }

  object PositionedNode {
    def apply[T](value: T, left: Tree[T], right: Tree[T], x: Int, y: Int) =
      new PositionedNode[T](value, left, right, x, y)

    def unapply[T](p: PositionedNode[T]) =
      Some((p.value, p.left, p.right, p.x, p.y))
  }

}
