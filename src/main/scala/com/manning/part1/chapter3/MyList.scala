package com.manning.part1.chapter3

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
}