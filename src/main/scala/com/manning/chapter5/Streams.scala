package com.manning.chapter5

object Streams {

  case object ManningEmpty extends ManningStream[Nothing]
  case class ManningCons[+A](h: () => A, t: () => ManningStream[A]) extends ManningStream[A]

  trait ManningStream[+A] {

    def toList: List[A] = this match {
      case ManningEmpty => List()
      case ManningCons(h, t) => h() :: t().toList
    }

    def take(n: Int): ManningStream[A] = this match {
      case ManningEmpty => ManningEmpty
      case ManningCons(h, t) =>
        if (n > 1) ManningStream.cons(h(), t().take(n - 1))
        else ManningStream.cons(h(), ManningEmpty)
    }
  }

  object ManningStream {

    def cons[A](head: => A, tail: => ManningStream[A]): ManningStream[A] = {
      lazy val h = head
      lazy val t = tail
      ManningCons(() => h, () => t)
    }

    def empty[A]: ManningStream[A] = ManningEmpty

    def apply[A](as: A*): ManningStream[A] = {
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))
    }
  }
}