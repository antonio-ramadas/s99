package s99

trait LogicAndCodesSolutions { outer =>

  implicit class ExtendedBoolean(a: Boolean) {
    def and(b: => Boolean): Boolean = outer.and(a, b)
    def or(b: => Boolean): Boolean = outer.or(a, b)
    def nand(b: => Boolean): Boolean = outer.nand(a, b)
    def nor(b: => Boolean): Boolean = outer.nor(a, b)
    def xor(b: => Boolean): Boolean = outer.xor(a, b)
    def impl(b: => Boolean): Boolean = outer.impl(a, b)
    def equ(b: => Boolean): Boolean = outer.equ(a, b)
  }
  
  def and(a: Boolean, b: => Boolean): Boolean = if (a) b else false
  def or(a: Boolean, b: => Boolean): Boolean = if (a) true else b
  def nand(a: Boolean, b: => Boolean): Boolean = not(and(a, b))
  def nor(a: Boolean, b: => Boolean): Boolean = not(or(a, b))
  def xor(a: Boolean, b: => Boolean): Boolean = or(and(a, not(b)), and(not(a), b))
  def impl(a: Boolean, b: => Boolean): Boolean = or(not(a), b)
  def equ(a: Boolean, b: => Boolean): Boolean = or(and(a, b), and(not(a), not(b)))
  def not(a: Boolean): Boolean = if (a) false else true

  def table2(f: (Boolean, Boolean) => Boolean): String = {
    val rows: List[(Boolean, Boolean)] = List((true, true), (true, false), (false, true), (false, false))

    def toString(a: Boolean): String = if (a) "true  " else "false "

    val v1 = rows.foldLeft("A     B     result\n"){case (acc, (a, b)) => acc + List(a, b, f(a,b)).map(toString).reduce(_ + _) + "\n"}

    val v2 = ("A     B     result" :: rows.map{case (a, b) => List(a, b, f(a,b)).map(toString).reduce(_ + _)}).mkString("\n")

    v2
  }

  def gray(n: Int): List[String] = ???
  def huffman(list: List[(String,  Int)]): List[(String, String)] = ???
}
