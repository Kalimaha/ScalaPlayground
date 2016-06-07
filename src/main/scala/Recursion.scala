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

  def map[A, B](as: List[A], f: A => B):List[B] = as match {
   case Nil => Nil
   case h :: t => f(h) :: map(t, f)
  }

  def filter[A](as: List[A], f: A => Boolean): List[A] = as match {
   case Nil => Nil
   case h :: t => {
    var tmp: List[A] = filter(t, f)
    if (f(h))
      h :: tmp
    else
      tmp
   }
  }

  def append[A](x: List[A], b: List[A]): List[A] = x match {
   case Nil => b
   case h :: t => h :: append(t, b)
  }

  def flatten[A](x: List[List[A]]): List[A] = {
    def loop(acc: List[A], l: List[List[A]]): List[A] = l match {
     case Nil => acc
     case h :: t => loop(append(acc, h), t)
    }
    loop(Nil, x)
  }

  def maximum(x: List[Int]): Int = {
    def loop(acc: Int, l: List[Int]): Int = l match {
     case Nil => acc
     case h :: t => if (acc > h) loop(acc, t) else loop(h, t)
    }
    loop(0, x)
  }

  def reverse[A](x: List[A]): List[A] = {
    def loop(acc: List[A], l: List[A]): List[A] = l match {
     case Nil => acc
     case h :: t => loop(h :: acc, t)
    }
    loop(Nil, x)
  }

}
