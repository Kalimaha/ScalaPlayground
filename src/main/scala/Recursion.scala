object Recursion {

  def plusOne(n: Int): Int = n + 1

  def minusOne(n: Int): Int = n - 1

  def add(a: Int, b: Int): Int =
    if (b == 0) a
    else add(plusOne(a), minusOne(b))

  def sum(l: List[Int]): Int = l match {
    case Nil => 0
    case head :: tail => head + sum(tail)
  }

  def length(l: List[Int]): Int = l match {
    case Nil => 0
    case head :: tail => 1 + length(tail)
  }

}