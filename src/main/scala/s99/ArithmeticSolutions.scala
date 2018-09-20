package s99

trait ArithmeticSolutions {

  // add new functions to integers
  implicit class ExtendedInt(n: Int) {
    def isPrime: Boolean = {
      def numbers(curr: Int): Stream[Int] = if (curr > 1) curr #:: numbers(curr-1) else Stream.empty

      if (n == 1) false else !numbers(n-1).exists(n % _ == 0)
    }

    def isCoprimeTo(n: Int): Boolean = 1 == gcd(this.n, n)

    def totient: Int = (1 to n).count(isCoprimeTo)

    def primeFactors: List[Int] = ???
    def primeFactorMultiplicity: List[(Int, Int)] = ???
    def primeFactorMultiplicityMap: Map[Int, Int] = ???
    def improvedTotient: Int = ???
    def listPrimesinRange(r: Range): List[Int] = ???
    def goldbach: (Int, Int) = ???
  }

  def gcd(m: Int, n: Int): Int = (m,n) match {
    case (a, 0) => a
    case (a, b) => gcd(b, a % b)
  }

  def listPrimesinRange(r: Range): List[Int] = ???
  def printGoldbachList(r: Range): List[String] = ???
  def printGoldbachListLimited(r: Range, limit: Int): List[String] = ???

  // Optional but possibly useful exercise: not in original s-99 problems
  def primes: Stream[Int] = ???
}
