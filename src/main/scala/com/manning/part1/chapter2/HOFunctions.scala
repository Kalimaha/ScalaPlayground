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
}