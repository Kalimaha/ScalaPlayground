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
    def loop(acc: Int)(l: MyList[A]): MyList[A] =
      if (acc == n) l
      else loop(1 + acc)(tail(l))
    loop(0)(l)
  }

  def append[A](l1: MyList[A], l2: MyList[A]): MyList[A] = l1 match {
    case MyNil => l2
    case MyCons(h, t) => MyCons(h, append(t, l2))
  }

  def dropWhile[A](f: A => Boolean)(l: MyList[A]): MyList[A] = l match {
    case MyNil => MyNil
    case MyCons(h, t) =>
      if (f(h)) dropWhile(f)(t)
      else MyCons(h, dropWhile(f)(t))
  }

  def init[A](l: MyList[A]): MyList[A] = l match {
    case MyNil => MyNil
    case MyCons(_, MyNil) => MyNil
    case MyCons(h, t) => MyCons(h, init(t))
  }

  @tailrec
  def foldLeft[A, B](l: MyList[A])(z: B)(f: (A, B) => B): B = l match {
    case MyNil => z
    case MyCons(h, t) => foldLeft(t)(f(h, z))(f)
  }

  def length[A](l: MyList[A]): Int = foldLeft(l)(0)((_, b: Int) => b + 1)

  def foldRight[A, B](l: MyList[A], z: B)(f: (A, B) => B): B = l match {
    case MyNil => z
    case MyCons(h, t) => f(h, foldRight(t, z)(f))
  }

  def appendWithFoldRight[A](l1: MyList[A])(l2: MyList[A]): MyList[A] = foldRight(l1, l2)(MyCons(_, _))

  def addOne(l: MyList[Int]): MyList[Int] = foldRight(l, MyList[Int]())((h, t) => MyCons(1 + h, t))

  def double2String(l: MyList[Double]): MyList[String] = foldRight(l, MyList[String]())((h, t) => MyCons(h.toString, t))

  def map[A, B](l: MyList[A])(f: A => B): MyList[B] = foldRight(l, MyList[B]())((h, t) => MyCons(f(h), t))

  def filter[A](l: MyList[A])(f: A => Boolean): MyList[A] = foldRight(l, MyList[A]())((h, t) => if (f(h)) MyCons(h, t) else t)

  def flatMap[A, B](l: MyList[A])(f: A => MyList[B]): MyList[B] = foldRight(map(l)(f), MyList[B]())(append)

  def filterWithFlatMap[A](l: MyList[A])(f: A => Boolean): MyList[A] = flatMap(l)(a => if (f(a)) MyList(a) else MyNil)

  def addPairwise(l1: MyList[Int], l2: MyList[Int]): MyList[Int] = (l1, l2) match {
    case (MyNil, _) => MyNil
    case (_, MyNil) => MyNil
    case (MyCons(h1, t1), MyCons(h2, t2)) => MyCons(h1 + h2, addPairwise(t1, t2))
  }

  def zipWith[A, B, C](l1: MyList[A], l2: MyList[B])(f: (A, B) => C): MyList[C] = (l1, l2) match {
    case (MyNil, _) => MyNil
    case (_, MyNil) => MyNil
    case (MyCons(h1, t1), MyCons(h2, t2)) => MyCons(f(h1, h2), zipWith(t1, t2)(f))
  }
}