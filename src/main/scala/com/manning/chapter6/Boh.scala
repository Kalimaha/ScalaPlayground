package com.manning.chapter6

object Boh {
  def alpha: String => Int => Double = (s: String) => (i: Int) => {
    println(s"Alpha received $s and $i.")
    1.0 * s.length / i
  }

  def beta(f: Int => Double): Double = f(42)

  def gamma = beta _ compose alpha

  def main(args: Array[String]) {
    println(gamma("Muro del Canto"))

    def g(i: Int) = s"g($i)"
    def f(s: String) = s"f($s)"
    def fg = f _ compose g
    println(fg(42))
  }
}