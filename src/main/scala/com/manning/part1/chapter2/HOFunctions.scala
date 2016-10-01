package com.manning.part1.chapter2

import scala.annotation.tailrec

object HOFunctions {
  def fib(n: Int): Int = n match {
    case 1 => 0
    case 2 => 1
    case _ => fib(n - 1) + fib(n - 2)
  }

  def isSorted[A](as: List[A], f: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(idx: Int): Boolean =
      if (idx == as.length - 1) true
      else f(as(idx), as(idx + 1)) && loop(idx + 1)
    loop(0)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => f(a, _)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}