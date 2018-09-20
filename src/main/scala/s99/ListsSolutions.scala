package s99

trait ListsSolutions {
  def last[T](list: List[T]): T = list match {
    case Nil => throw new Exception("There is no last element of empty list")
    case x :: Nil => x
    case _ :: xs => last(xs)
  }

  def penultimate[T](list: List[T]): T = list match {
    case Nil => throw new Exception("There is no penultimate element of empty list")
    case x :: Nil => throw new Exception("There is no penultimate element of a single element list")
    case x :: _ :: Nil => x
    case _ :: xs => penultimate(xs)
  }

  def nth[T](n: Int, list: List[T]): T = if (n == 0) list.head else nth(n - 1, list.tail)

  def length[T](list: List[T]): Int = list match {
    case Nil => 0
    case _ :: xs => 1 + length(xs)
  }

  def reverse[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case x :: xs => reverse(xs) ::: List(x)
  }

  def isPalindrome[T](list: List[T]): Boolean = reverse(list) == list

  def flatten(list: List[Any]): List[Any] = list match {
    case Nil => Nil
    case (x: List[Any]) :: xs => flatten(x) ::: flatten(xs)
    case x :: xs => x :: flatten(xs)
  }

  def compress[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case x :: x1 :: xs if x == x1 => compress(x1 :: xs)
    case x :: xs => x :: compress(xs)
  }

  def pack[T](list: List[T]): List[List[T]] = ???

  def encode[T](list: List[T]): List[(Int, T)] = ???

  def encodeModified[T](list: List[T]): List[Any] = ???

  def decode[T](list: List[(Int, T)]): List[T] = ???

  def encodeDirect[T](list: List[T]): List[(Int, T)] = ???

  def duplicate[T](list: List[T]): List[T] = ???

  def duplicateN[T](n: Int, list: List[T]): List[T] = ???

  def drop[T](n: Int, list: List[T]): List[T] = ???

  def split[T](n: Int, list: List[T]): (List[T], List[T]) = ???

  def slice[T](i: Int, j: Int, list: List[T]): List[T] = ???

  def rotate[T](n: Int, list: List[T]): List[T] = ???

  def removeAt[T](i: Int, list: List[T]): (List[T], T) = ???

  def insertAt[T](t: T, i: Int, list: List[T]): List[T] = ???

  def range[T](i: Int, j: Int): List[Int] = ???

  def randomSelect[T](n: Int, list: List[T]): List[T] = ???

  def lotto[T](i: Int, j: Int): List[Int] = ???

  def randomPermute[T](list: List[T]): List[T] = ???

  def combinations[T](n: Int, list: List[T]): List[List[T]] = ???

  def group3[T](list: List[T]): List[List[List[T]]] = ???

  def groups[T](ns: List[Int], list: List[T]): List[List[List[T]]] = ???

  def lsort[T](list: List[List[T]]): List[List[T]] = ???

  def lsortFreq[T](list: List[List[T]]): List[List[T]] = ???
}
