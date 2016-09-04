package com.manning

import scala.annotation.tailrec

object Chapter3 {

  sealed trait ManningList[+A]
    case object Nil extends ManningList[Nothing]
    case class Cons[+A](head: A, tail: ManningList[A]) extends ManningList[A]

  sealed trait ManningTree[+A]
    case class Leaf[+A](value: A) extends ManningTree[A]
    case class Branch[+A](left: ManningTree[A], right: ManningTree[A]) extends ManningTree[A]

  object ManningTree {

    def size[A](tree: ManningTree[A]): Int = tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

    def maximum(tree: ManningTree[Int]): Int = tree match {
      case Leaf(a) => a
      case Branch(l, r) => maximum(l) max maximum(r)
    }

    def depth[A](tree: ManningTree[A]): Int = tree match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

    def map[A, B](tree: ManningTree[A])(f: A => B): ManningTree[B] = tree match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch.apply(left = map(l)(f), right = map(r)(f))
    }

    def fold[A, B](tree: ManningTree[A])(f: A => B)(g: (B, B) => B): B = tree match {
      case Leaf(a) => f(a)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    def sizeWithFold[A](tree: ManningTree[A]): Int = fold(tree)(a => 1)(1 + _ + _)

    def maximumWithFold(tree: ManningTree[Int]): Int = fold(tree)(a => a)(_ max _)

    def depthWithFold[A](tree: ManningTree[A]): Int = fold(tree)(a => 0)((a1, a2) => 1 + (a1 max a2))
  }

  object ManningList {

    def apply[A](as: A*): ManningList[A] = {
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }

    def tail[A](l: ManningList[A]): ManningList[A] = l match {
      case Nil => Nil
      case Cons(_, t) => t
    }

    def setHead[A](l: ManningList[A], a: A): ManningList[A] = l match {
      case Nil => Nil
      case Cons(h, t) => Cons(a, t)
    }

    def drop[A](l: ManningList[A], n: Int): ManningList[A] = {
      @tailrec
      def loop(l: ManningList[A], count: Int): ManningList[A] = {
        if (count == n) l
        else loop(tail(l), 1 + count)
      }
      loop(l, 0)
    }

    def dropWhile[A](l: ManningList[A], f: A => Boolean): ManningList[A] = l match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) Cons(h, dropWhile(t, f)) else dropWhile(t, f)
    }

    def append[A](l1: ManningList[A], l2: ManningList[A]): ManningList[A] = l1 match {
      case Nil => l2
      case Cons(h, t) => Cons(h, append(t, l2))
    }

    def foldRight[A, B](l: ManningList[A], z: B)(f: (A, B) => B): B = l match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

    def length[A](l: ManningList[A]): Int = {
      foldLeft(l, 0)((tot: Int, _) => 1 + tot)
    }

    def sum(l: ManningList[Int]): Int = {
      foldLeft(l, 0)((tot: Int, current: Int) => current + tot)
    }

    def product(l: ManningList[Int]): Int = {
      foldLeft(l, 1)((tot: Int, current: Int) => current * tot)
    }

    @tailrec
    def foldLeft[A, B](l: ManningList[A], z: B)(f: (B, A) => B): B = l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

    def concat[A](ll: ManningList[ManningList[A]]): ManningList[A] = ll match {
      case Nil => Nil
      case Cons(h, t) => append(h, concat(t))
    }

    def addOne(l: ManningList[Int]): ManningList[Int] = l match {
      case Nil => Nil
      case Cons(h, t) => Cons(1 + h, addOne(t))
    }

    def toString(l: ManningList[Double]): ManningList[String] = l match {
      case Nil => Nil
      case Cons(h, t) => Cons(h.toString, toString(t))
    }

    def map[A, B](l: ManningList[A])(f: A => B): ManningList[B] = l match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }

    def filter[A](l: ManningList[A])(f: A => Boolean): ManningList[A] = {
      dropWhile(l, f)
    }

    def filterFlatMap[A](l: ManningList[A])(f: A => Boolean): ManningList[A] = {
      flatMap(l)(a => if (f(a)) ManningList(a) else Nil)
    }

    def flatMap[A, B](l: ManningList[A])(f: A => ManningList[B]): ManningList[B] = {
      concat(map(l)(f))
    }

    def addPairWise(l1: ManningList[Int], l2: ManningList[Int]): ManningList[Int] = (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairWise(t1, t2))
    }

    def zipWith[A, B, C](l1: ManningList[A], l2: ManningList[B])(f: (A, B) => C): ManningList[C] = (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
  }
}