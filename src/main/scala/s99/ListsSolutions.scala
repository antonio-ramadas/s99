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

  def pack[T](list: List[T]): List[List[T]] = list match {
    case Nil => Nil
    case z :: zs =>
      def packAux(l: List[T], curr: List[T]): (List[T], List[T]) = (l, curr) match {
        case (Nil, xs) => (Nil, xs)
        case (x :: xs, y :: ys) if x == y => packAux(xs, x :: y :: ys)
        case _ => (l, curr)
      }

      val (l, curr) = packAux(zs, List(z))

      curr :: pack(l)
  }

  def encode[T](list: List[T]): List[(Int, T)] = {
    def enc(l: List[List[T]]): List[(Int, T)] = l match {
      case Nil => Nil
      case x :: xs => (x.size, x.head) :: enc(xs)
    }

    val packed = pack(list)

    enc(packed)
  }

  def encodeModified[T](list: List[T]): List[Any] = {
    val encoded = encode(list)

    def encM(list: List[(Int, T)]): List[Any] = list match {
      case Nil => Nil
      case (i, el) :: xs => (if (i == 1) el else (i, el)) :: encM(xs)
    }

    encM(encoded)
  }

  def decode[T](list: List[(Int, T)]): List[T] = list match {
    case Nil => Nil
    case (i, el) :: xs =>

      def dec(i: Int, el: T): List[T] = if (i == 0) Nil else el :: dec(i-1, el)

      dec(i, el) ::: decode(xs)

  }

  def encodeDirect[T](list: List[T]): List[(Int, T)] = list match {
    case Nil => Nil
    case x :: _ => (list.takeWhile(x == _).size, x) :: encodeDirect(list.dropWhile(x == _))
  }

  def duplicate[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case x :: xs => x :: x :: duplicate(xs)
  }

  def duplicateN[T](n: Int, list: List[T]): List[T] = list match {
    case Nil => Nil
    case x :: xs =>  List.fill(n)(x) ::: duplicateN(n, xs)
  }

  def drop[T](n: Int, list: List[T]): List[T] = {
    def dropAux(left: Int, l: List[T]): List[T] = l match {
      case Nil => Nil
      case x :: xs => if (left == 1) dropAux(n, xs) else x :: dropAux(left-1, xs)
    }
    dropAux(n, list)
  }

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
