package com.manning.chapter5

import scala.annotation.tailrec

object Streams {

  case object ManningEmpty extends ManningStream[Nothing]
  case class ManningCons[+A](h: () => A, t: () => ManningStream[A]) extends ManningStream[A]

  def constant(i: Int): ManningStream[Int] = ManningStream.cons(i, constant(i))

  def from(i: Int): ManningStream[Int] = ManningStream.cons(i, from(i + 1))

  def fibonacci: ManningStream[Int] = {
    def loop(i0: Int, i1: Int): ManningStream[Int] = ManningStream.cons(i0, loop(i1, i0 + i1))
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): ManningStream[A] = f(z) match {
      case Some((h, s)) => ManningStream.cons(h, unfold(s)(f))
      case None => ManningStream.empty
  }

  trait ManningStream[+A] {

    import ManningStream._

    def toList: List[A] = this match {
      case ManningEmpty => List()
      case ManningCons(h, t) => h() :: t().toList
    }

    def take(n: Int): ManningStream[A] = this match {
      case ManningEmpty => ManningEmpty
      case ManningCons(h, t) =>
        if (n > 1) cons(h(), t().take(n - 1))
        else cons(h(), ManningEmpty)
    }

    @tailrec
    final def drop(n: Int): ManningStream[A] = this match {
      case ManningEmpty => ManningEmpty
      case ManningCons(h, t) =>
        if (n > 0) t().drop(n - 1)
        else ManningCons(h, t)
    }

    def takeWhile(f: A => Boolean): ManningStream[A] = this match {
      case ManningEmpty => ManningEmpty
      case ManningCons(h, t) =>
        if (f(h())) cons(h(), t() takeWhile f)
        else t() takeWhile f
    }

    def takeWhile2(f: A => Boolean): ManningStream[A] = {
      foldRight(empty[A])((a, b) =>
        if (f(a)) cons(a, b)
        else empty
      )
    }

    @tailrec
    final def exists(f: A => Boolean): Boolean = this match {
      case ManningEmpty => false
      case ManningCons(h, t) => f(h()) || t().exists(f)
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case ManningCons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists2(f: A => Boolean): Boolean = foldRight(false)((a, b) => f(a) || b)

    def forAll(f: A => Boolean): Boolean = foldRight(true)((a, b) => f(a) && b)

    def map[B](f: A => B): ManningStream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

    def filter(f: A => Boolean): ManningStream[A] = foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

    def append[B >: A](s: => ManningStream[B]): ManningStream[B] = foldRight(s)((h, t) => cons(h, t))

    def flatMap[B](f: A => ManningStream[B]): ManningStream[B] = foldRight(empty[B])((a, b) => f(a) append b)

    def head: Option[A] = this match {
      case ManningEmpty => None
      case ManningCons(h, t) => Some(h())
    }

    def find(f: A => Boolean): Option[A] = filter(f).head
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