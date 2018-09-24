package s99

trait MultiwayTreesSolutions {

  case class MTree[+T](value: T, children: List[MTree[T]] = Nil) {
    def nodeCount: Int = 1 + children.map(_.nodeCount).sum // 1 + children.foldLeft(0)(_ + _.nodeCount)

    def show: String = s"$value${children.map(_.show).mkString}^"

    def internalPathLength: Int = ???

    def postOrder: List[T] = ???

    def lispyTree: String = ???

    override def toString = s"M($value ${children.mkString(",")})"
  }

  object MTree {
    def fromLispyTree(string: String): MTree[Char] = ???
  }

  implicit def string2MTree(s: String): MTree[Char] = {
    def aux(str: String, stack: List[MTree[Char]]): MTree[Char] = {
      if (str.isEmpty)
        stack.head
      else if (str.head == '^') {
        val curr = stack.head
        val parent = stack.tail.head

        val newChildren = curr :: parent.children.filterNot(_.value == curr.value)

        val newParent = MTree(parent.value, newChildren.sortBy(_.value))

        aux(str.tail, newParent :: stack.tail.tail)
      } else {
        val curr = MTree(str.head)

        val parent = stack.head
        val newParent = MTree(parent.value, curr :: parent.children)

        aux(str.tail, curr :: newParent :: stack.tail)
      }
    }

    aux(s.substring(1, s.length-1), List(MTree(s.head)))
  }
}
