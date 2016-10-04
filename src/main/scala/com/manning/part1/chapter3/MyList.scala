package com.manning.part1.chapter3

import scala.annotation.tailrec


sealed trait MyList[+A]
  case object MyNil extends MyList[Nothing]
  case class  MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) MyNil
    else MyCons(as.head, apply(as.tail: _*))

  def sum(l: MyList[Int]): Int = l match {
    case MyNil => 0
    case MyCons(h, t) => h + sum(t)
  }

  def product(l: MyList[Double]): Double = l match {
    case MyNil => 1
    case MyCons(h, t) => h * product(t)
  }

  def tail[A](l: MyList[A]): MyList[A] = l match {
    case MyNil => MyNil
    case MyCons(_, t) => t
  }

  def tailWithDrop[A](l: MyList[A]): MyList[A] = drop(l)(1)

  def setHead[A](l: MyList[A])(a: A): MyList[A] = l match {
    case MyNil => MyNil
    case MyCons(_, t) => MyCons(a, t)
  }

  def drop[A](l: MyList[A])(n: Int): MyList[A] = {
    @tailrec
    def loop(acc: Int)(l: MyList[A]): MyList[A] = {
      if (acc == n) l
      else loop(1 + acc)(tail(l))
    }
    loop(0)(l)
  }

  def append[A](l1: MyList[A])(l2: MyList[A]): MyList[A] = l1 match {
    case MyNil => l2
    case MyCons(h, t) => MyCons(h, append(t)(l2))
  }

  def dropWhile[A](f: A => Boolean)(l: MyList[A]): MyList[A] = l match {
    case MyNil => MyNil
    case MyCons(h, t) => if (f(h)) dropWhile(f)(t) else MyCons(h, dropWhile(f)(t))
  }
}