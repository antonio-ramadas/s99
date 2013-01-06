package s99

import math._
import Solutions._

trait BinaryTreesSolutions {

  sealed abstract class Tree[+T] {
    def isMirrorOf(t: Tree[_]): Boolean
    def isSymmetric: Boolean
    def addValue[S >: T <% Ordered[S]](s: S): Tree[S]

    def nodeCount: Int
    def leafCount: Int
    def leafList: List[T]
    def internalList: List[T]
    def atLevel(n: Int): List[T]

    def preOrder: List[T] = ???
    def inOrder: List[T] = ???
    def toDotString: String = ???
  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

    def isMirrorOf(t1: Tree[_]): Boolean = t1 match {
      case End => false
      case Node(_, thatLeft, thatRight) =>
        left.isMirrorOf(thatRight) && right.isMirrorOf(thatLeft)
    }

    def isSymmetric: Boolean = left.isMirrorOf(right)

    def addValue[S >: T <% Ordered[S]](s: S): Tree[S] =
      if (s < value) Node(value, left.addValue(s), right)
      else Node(value, left, right.addValue(s))

    def nodeCount: Int = 1 + left.nodeCount + right.nodeCount

    def leafCount: Int =
      if (left == End && right == End) 1
      else left.leafCount + right.leafCount

    def leafList: List[T] =
      if (left == End && right == End) List(value)
      else left.leafList ::: right.leafList

    def internalList: List[T] =
      if (left == End && right == End) Nil
      else value :: left.internalList ::: right.internalList

    def atLevel(n: Int): List[T] =
      if (n == 1) List(value)
      else left.atLevel(n - 1) ::: right.atLevel(n - 1)

    def layoutBinaryTree: PositionedNode[T] = ???
    def layoutBinaryTree2: PositionedNode[T] = ???
    def layoutBinaryTree3: PositionedNode[T] = ???

    def show: String = ???
  }

  case object End extends Tree[Nothing] {
    override def toString = "."

    def isMirrorOf(t1: Tree[_]): Boolean = (t1 == End)
    def isSymmetric: Boolean = true
    def addValue[S <% Ordered[S]](s: S): Tree[S] = Node(s)

    def nodeCount: Int = 0
    def leafCount: Int = 0
    def leafList: List[Nothing] = Nil
    def internalList: List[Nothing] = Nil
    def atLevel(n: Int): List[Nothing] = Nil
  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }

  object Tree {

    def cBalanced[T](n: Int, e: T): List[Tree[T]] =
      if (n == 0) List(End)
      else if ((n - 1) % 2 == 0) {
        val child = cBalanced((n - 1) / 2, e)
        for (left <- child; right <- child) yield Node(e, left, right)
      } else for {
        small <- cBalanced((n - 1) / 2, e)
        big <- cBalanced((n - 1) / 2 + 1, e)
        (left, right) <- List((small, big), (big, small))
      } yield Node(e, left, right)

    def fromList[T <% Ordered[T]](list: List[T]): Tree[T] =
      list.foldLeft(End: Tree[T])(_.addValue(_))

    def symmetricBalancedTrees[T](n: Int, e: T): List[Tree[T]] =
      cBalanced(n, e).filter(_.isSymmetric)

    def hbalTrees[T](h: Int, e: T): List[Tree[T]] =
      if (h < 0) Nil
      else if (h == 0) List(End)
      else {
        val talls = hbalTrees(h - 1, e)
        val balanced = for (left <- talls; right <- talls)
          yield Node(e, left, right)
        val unbalanced = for {
          tall <- talls
          short <- hbalTrees(h - 2, e)
          (left, right) <- List((short, tall), (tall, short))
        } yield Node(e, left, right)

        balanced ::: unbalanced
      }

    def minHbalNodes(n: Int): Int = {
      def minHbalNodesAux(n: Int, curr: Int, last: Int): Int =
        if (n == 0) curr
        else minHbalNodesAux(n - 1, curr + last + 1, curr)

      minHbalNodesAux(n, 0, 0)
    }

//    // non-tail recursive formulation
//    def minHbalNodes(h: Int): Int =
//      if (h == 0) 0
//      else if (h == 1) 1
//      else 1 + minHbalNodes(h - 1) + minHbalNodes(h - 2)

    def maxHbalHeight(n: Int): Int =
      Stream.from(1).dropWhile(minHbalNodes(_) < n).head

//    // recursive version
//    def maxHbalHeight(n: Int): Int = {
//      def maxHbalHeightAux(h: Int, curr: Int, last: Int): Int =
//        if (n < curr) h - 1
//        else maxHbalHeightAux(h + 1, curr + last + 1, curr)
//
//      maxHbalHeightAux(0, 0, 0)
//    }

    def hbalTreesWithNodes[T](n: Int, e: T): List[Tree[T]] = {
      val maxH = maxHbalHeight(n)
      val minH = (log(n) / log(2)).toInt
      (minH to maxH).flatMap(hbalTrees(_, e)).toList.filter(_.nodeCount == n)
    }

    def completeBinaryTree[T](n: Int, e: T): Tree[T] = {
      if (n == 0) End
      else {
        val fullTreeNodes = pow(2, (log(n + 1) / log(2)).toInt).toInt - 1
        val fullChildTreeNodes = (fullTreeNodes - 1) / 2
        val rem = n - fullTreeNodes
        val leftChildRem = min(rem, (fullTreeNodes + 1) / 2)
        Node(e,
          completeBinaryTree(fullChildTreeNodes + leftChildRem, e),
          completeBinaryTree(fullChildTreeNodes + rem - leftChildRem, e)
        )
      }
    }

    def fromString(string: String): Node[Char] = ???
    def fromDotString(string: String): Node[Char] = ???

    def string2Tree(string: String): Tree[Char] = ???

    def preInTree(lists: List[Char]*): Node[Char] = ???
  }

  class PositionedNode[+T](override val value: T,
                           override val left: Tree[T],
                           override val right: Tree[T], x: Int, y: Int) extends Node[T](value, left, right) {
    override def toString = "T[" + x.toString + "," + y.toString + "](" +
      value.toString + " " + left.toString + " " + right.toString + ")"
  }

}
