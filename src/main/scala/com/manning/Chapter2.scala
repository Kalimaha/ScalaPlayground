package com.manning

import scala.annotation.tailrec

object Chapter2 {

  def factorial(n: Int): Int = {
    if (n == 0) 1
    else n * factorial(n - 1)
  }

  def findFirst[A](l: List[A], a: A): Int = {
    @tailrec
    def loop(currentIndex: Int): Int = {
      if (currentIndex >= l.size) -1
      else if (l(currentIndex).equals(a)) currentIndex
      else loop(currentIndex + 1)
    }
    loop(0)
  }

  def fibonacci(n: Int): Int = {
    if (n == 1 || n == 2) n - 1
    else fibonacci(n - 1) + fibonacci(n - 2)
  }

  def isSorted[A](as: List[A], f: (A, A) => Boolean) = {
    @tailrec
    def loop(currentIndex: Int): Boolean = {
      if (currentIndex >= as.size) true
      else f(as(currentIndex - 1), as(currentIndex)) && loop(1 + currentIndex)
    }
    loop(1)
  }
}