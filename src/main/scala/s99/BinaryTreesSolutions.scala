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
    def height: Int

    def layoutBinaryTree: Tree[T] = layoutBinaryTreeAux(1, 1)._1
    protected[s99] def layoutBinaryTreeAux(x: Int, y: Int): (Tree[T], Int)

    def layoutBinaryTree2: Tree[T] = {
      val h = height
      val rootX = 1 + (1 until leftPathHeight).map(i => 1 << (h - i - 1)).sum
      layoutBinaryTree2Aux(rootX, 1, h)
    }

    protected[s99] def leftPathHeight: Int
    protected[s99] def layoutBinaryTree2Aux(x: Int, y: Int, h: Int): Tree[T]

    def layoutBinaryTree3: Tree[T]
    protected[s99] def layoutBinaryTree3Aux(x: Int, y: Int): Tree[T]
    protected[s99] def levelRanges: List[(Int, Int)]
    protected[s99] def translate(dx: Int, dy: Int = 0): Tree[T]

    def show: String = ???
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

    def height: Int = max(left.height, right.height) + 1

    protected[s99] def layoutBinaryTreeAux(x: Int, y: Int): (PositionedNode[T], Int) = {
      val (newLeft, leftPos) = left.layoutBinaryTreeAux(x, y + 1)
      val (newRight, rightPos) = right.layoutBinaryTreeAux(leftPos + 1, y + 1)
      (PositionedNode(value, newLeft, newRight, leftPos, y), rightPos)
    }

    protected[s99] def leftPathHeight: Int = left.leftPathHeight + 1

    protected[s99] def layoutBinaryTree2Aux(x: Int, y: Int, h: Int): Tree[T] =
      PositionedNode(value,
        left.layoutBinaryTree2Aux(x - (1 << (h - 2)), y + 1, h - 1),
        right.layoutBinaryTree2Aux(x + (1 << (h - 2)), y + 1, h - 1),
        x, y)

    def layoutBinaryTree3: Tree[T] = {
      val newTree = layoutBinaryTree3Aux(0, 1)
      val leftBound = newTree.levelRanges.minBy(_._1)._1
      if(leftBound > 0) newTree
      else newTree.translate(-leftBound + 1)
    }

    protected[s99] def layoutBinaryTree3Aux(x: Int, y: Int): Tree[T] = {
      val newLeft = left.layoutBinaryTree3Aux(x - 1, y + 1)
      val newRight = right.layoutBinaryTree3Aux(x + 1, y + 1)
      val gap = newLeft.levelRanges.zip(newRight.levelRanges).foldLeft(0) {
        case (g, ((_, a2), (b1, _))) =>
          if(a2 - g < b1 + g) g
          else (a2 - b1) / 2 + 1
      }
      PositionedNode(value, newLeft.translate(-gap), newRight.translate(gap), x, y)
    }

    protected[s99] def levelRanges: List[(Int, Int)] =
      throw new UnsupportedOperationException

    protected[s99] def translate(dx: Int, dy: Int): Tree[T] =
      if(dx == 0 && dy == 0) this
      else Node(value, left.translate(dx, dy), right.translate(dx, dy))
  }

  case object End extends Tree[Nothing] {
    override def toString = "."

    def isMirrorOf(t1: Tree[_]) = (t1 == End)
    def isSymmetric = true
    def addValue[S <% Ordered[S]](s: S) = Node(s)

    def nodeCount = 0
    def leafCount = 0
    def leafList = Nil
    def internalList = Nil
    def atLevel(n: Int) = Nil
    def height = 0

    protected[s99] def layoutBinaryTreeAux(x: Int, y: Int) = (End, x)
    protected[s99] def leftPathHeight = 0
    protected[s99] def layoutBinaryTree2Aux(x: Int, y: Int, h: Int) = End

    def layoutBinaryTree3 = End
    protected[s99] def layoutBinaryTree3Aux(x: Int, y: Int) = End
    protected[s99] def levelRanges = Nil
    protected[s99] def translate(dx: Int, dy: Int) = End
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

    def fromString(string: String): Tree[Char] = ???
    def preInTree[T](pre: List[T], in: List[T]): Tree[T] = ???
    def fromDotString(string: String): Tree[Char] = ???
  }

  class PositionedNode[+T](override val value: T,
                           override val left: Tree[T],
                           override val right: Tree[T],
                           val x: Int,
                           val y: Int) extends Node[T](value, left, right) {
    override def toString = "T[" + x.toString + "," + y.toString + "](" +
      value.toString + " " + left.toString + " " + right.toString + ")"

    override protected[s99] def levelRanges: List[(Int, Int)] = {
      (x, x) :: left.levelRanges.zipAll(right.levelRanges, null, null).map {
        case (null, b) => b
        case (a, null) => a
        case ((a1, a2), (b1, b2)) => (min(a1, b1), min(a2, b2))
      }
    }

    override protected[s99] def translate(dx: Int, dy: Int): Tree[T] =
      PositionedNode(value, left.translate(dx, dy), right.translate(dx, dy), x + dx, y + dy)
  }

  object PositionedNode {
    def apply[T](value: T, left: Tree[T], right: Tree[T], x: Int, y: Int) =
      new PositionedNode[T](value, left, right, x, y)
    def unapply[T](p: PositionedNode[T]) =
      Some((p.value, p.left, p.right, p.x, p.y))
  }
}
