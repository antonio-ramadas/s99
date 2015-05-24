package s99

trait ArithmeticSolutions {

  // add new functions to integers
  implicit class ExtendedInt(n: Int) {

    def isPrime: Boolean =
      if (n == 1) false
      else if (n == 2 || n == 3) true
      else if (n % 6 != 1 && n % 6 != 5) false
      else (3 until n by 2).forall(n % _ != 0)

    def isCoprimeTo(m: Int): Boolean = gcd(n, m) == 1

    def totient: Int = (1 to n).count(_.isCoprimeTo(n))

    def primeFactors: List[Int] = {
      def primeFactorsAux(n: Int, i: Int): List[Int] =
        if (n == 1) Nil
        else if (n % i == 0) i :: primeFactorsAux(n / i, i)
        else primeFactorsAux(n, i + 1)

      primeFactorsAux(n, 2)
    }

    def primeFactorMultiplicity: List[(Int, Int)] =
      new ListsSolutions {}.encode(n.primeFactors).map(_.swap)

    def primeFactorMultiplicityMap: Map[Int, Int] = {
      def primeFactorsAux(n: Int, i: Int, acc: Map[Int, Int]): Map[Int, Int] =
        if (n == 1) acc
        else if (n % i == 0)
          primeFactorsAux(n / i, i, acc + (i -> (acc.getOrElse(i, 0) + 1)))
        else primeFactorsAux(n, i + 1, acc)

      primeFactorsAux(n, 2, Map())
    }

    def improvedTotient: Int = primeFactorMultiplicity.map {
      case (p, m) => (p - 1) * math.pow(p, m - 1).toInt
    }.foldLeft(1)(_ * _)

    def goldbach: (Int, Int) = {
      val prime = listPrimesinRange(2 to n).dropWhile { i =>
        !i.isPrime || !(n - i).isPrime
      }
      (prime.head, n - prime.head)
    }
  }

  def gcd(m: Int, n: Int): Int =
    if (n == 0) m
    else gcd(n, m % n)

  def listPrimesinRange(r: Range): List[Int] = r.filter(_.isPrime).toList

  def printGoldbachList(r: Range): List[String] = r.foldRight(List[String]()) { (i, acc) =>
    if (i <= 2 || i % 2 != 0) acc
    else {
      val (gb1, gb2) = i.goldbach
      "%d = %d + %d".format(i, gb1, gb2) :: acc
    }
  }

  def printGoldbachListLimited(r: Range, limit: Int): List[String] =
    r.foldRight(List[String]()) { (i, acc) =>
      if (i <= 2 || i % 2 != 0) acc
      else {
        val (gb1, gb2) = i.goldbach
        if (gb1 > limit) "%d = %d + %d".format(i, gb1, gb2) :: acc
        else acc
      }
    }

  // Optional but possibly useful exercise: not in original s-99 problems
  def primes: Stream[Int] = {
    def nextPrime(n: Int): Stream[Int] =
      (n + 1 to Int.MaxValue).dropWhile(!_.isPrime).headOption match {
        case Some(p) => p #:: nextPrime(p)
        case None => Stream.empty
      }
    nextPrime(1)
  }
}
