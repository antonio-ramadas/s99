package s99

import Solutions._

trait ListsSolutions {

  def last[T](list: List[T]): T = list match {
    case x :: Nil => x
    case _ :: xs => last(xs)
    case Nil => throw new NoSuchElementException
  }

  def penultimate[T](list: List[T]): T = list match {
    case x :: _ :: Nil => x
    case _ :: xs => penultimate(xs)
    case Nil => throw new NoSuchElementException
  }

  def nth[T](n: Int, list: List[T]): T =
    if (n == 0) list.head
    else nth(n - 1, list.tail)

  def length[T](list: List[T]): Int = list match {
    case Nil => 0
    case x :: xs => length(xs) + 1
  }

  //  // tailrec version
  //  def length[T](list: List[T]): Int = {
  //    def lengthAux(list: List[T], acc: Int): Int = list match {
  //      case Nil => acc
  //      case x :: xs => lengthAux(xs, acc + 1)
  //    }
  //    lengthAux(list, 0)
  //  }

  def reverse[T](list: List[T]): List[T] = {
    def reverseAux(list: List[T], acc: List[T]): List[T] = list match {
      case Nil => acc
      case x :: xs => reverseAux(xs, x :: acc)
    }
    reverseAux(list, Nil)
  }

  def isPalindrome[T](list: List[T]): Boolean = list == reverse(list)

  def flatten(list: List[Any]): List[Any] = {
    def flattenAux(elem: Any): List[Any] = elem match {
      case Nil => Nil
      case x :: xs => flattenAux(x) ::: flattenAux(xs)
      case x => List(x)
    }
    flattenAux(list)
  }

  def compress[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case x :: x1 :: xs if x == x1 => compress(x1 :: xs)
    case x :: xs => x :: compress(xs)
  }

  def pack[T](list: List[T]): List[List[T]] = {
    def packAux(list: List[T], curr: List[T]): List[List[T]] = (list, curr) match {
      case (Nil, Nil) => Nil
      case (Nil, cs) => cs :: Nil
      case (x :: xs, Nil) => packAux(xs, List(x))
      case (x :: xs, c :: cs) =>
        if (x == c) packAux(xs, x :: c :: cs)
        else curr :: packAux(xs, List(x))
    }
    packAux(list, Nil)
  }

  def encode[T](list: List[T]): List[(Int, T)] =
    pack(list).map { x => (length(x), x.head) }

//  // without using map (has errors)
//  def encode[T](list: List[T]): List[(Int, T)] = pack(list) match {
//    case Nil => Nil
//    case x :: xs => (length(x), x.head) :: encode(xs)
//  }

  def encodeModified[T](list: List[T]): List[Any] = encode(list).collect {
    case (1, x) => x
    case x => x
  }

  def decode[T](list: List[(Int, T)]): List[T] = list match {
    case Nil => Nil
    case (1, x) :: xs => x :: decode(xs)
    case (n, x) :: xs => x :: decode((n - 1, x) :: xs)
  }

  def encodeDirect[T](list: List[T]): List[(Int, T)] = {
    def encodeDirAux(list: List[T], curr: (Int, T)): List[(Int, T)] = list match {
      case Nil => List(curr)
      case x :: xs =>
        if (x == curr._2) encodeDirAux(xs, (curr._1 + 1, x))
        else curr :: encodeDirAux(xs, (1, x))
    }
    list match {
      case Nil => Nil
      case x :: xs => encodeDirAux(xs, (1, x))
    }
  }

  def duplicate[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case x :: xs => x :: x :: duplicate(xs)
  }

  def duplicateN[T](n: Int, list: List[T]): List[T] = {
    def duplicateAux(left: Int, list: List[T]): List[T] = list match {
      case Nil => Nil
      case x :: xs =>
        if (left == 0) duplicateAux(n, xs)
        else x :: duplicateAux(left - 1, x :: xs)
    }
    duplicateAux(n, list)
  }

  def drop[T](n: Int, list: List[T]): List[T] = {
    def dropAux(k: Int, list: List[T]): List[T] = list match {
      case Nil => Nil
      case x :: xs =>
        if(k == 1) dropAux(n, xs)
        else x :: dropAux(k - 1, xs)
    }
    dropAux(n, list)
  }

  def split[T](n: Int, list: List[T]): (List[T], List[T]) =
    if(n == 0) (Nil, list)
    else {
      val (left, right) = split(n - 1, list.tail)
      (list.head :: left, right)
    }

  def slice[T](i: Int, j: Int, list: List[T]): List[T] =
    if(i > 0) slice(i - 1, j - 1, list.tail)
    else if (j > 0) list.head :: slice(i, j - 1, list.tail)
    else Nil

  def rotate[T](n: Int, list: List[T]): List[T] =
    if (list.isEmpty) Nil
    else {
      val len = length(list)
      val pos = n % len
      val (left, right) = split(if(pos < 0) pos + len else pos, list)
      right ::: left
    }

  def removeAt[T](i: Int, list: List[T]): (List[T], T) =
    if (i == 0) (list.tail, list.head)
    else {
      val (rem, elem) = removeAt(i - 1, list.tail)
      (list.head :: rem, elem)
    }

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

