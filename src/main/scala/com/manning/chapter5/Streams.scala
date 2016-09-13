package com.manning.chapter5

import scala.annotation.tailrec

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
        if (f(h())) ManningStream.cons(h(), t() takeWhile f)
        else t() takeWhile f
    }

    def takeWhile2(f: A => Boolean): ManningStream[A] = {
      foldRight(ManningStream.empty[A])((a, b) =>
        if (f(a)) ManningStream.cons(a, b)
        else ManningStream.empty
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

    def map[B](f: A => B): ManningStream[B] = foldRight(ManningStream.empty[B])((a, b) => ManningStream.cons(f(a), b))

    def filter(f: A => Boolean): ManningStream[A] = foldRight(ManningStream.empty[A])((a, b) => if (f(a)) ManningStream.cons(a, b) else b)

    def append[B >: A](s: => ManningStream[B]): ManningStream[B] = foldRight(s)((h, t) => ManningStream.cons(h, t))

    def flatMap[B](f: A => ManningStream[B]): ManningStream[B] = foldRight(ManningStream.empty[B])((a, b) => f(a) append b)
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