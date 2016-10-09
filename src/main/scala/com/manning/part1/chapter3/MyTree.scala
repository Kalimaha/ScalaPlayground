package com.manning.part1.chapter3

sealed trait MyTree[+A]
  case class MyLeaf[A](value: A) extends MyTree[A]
  case class MyBranch[A](left: MyTree[A], right: MyTree[A]) extends MyTree[A]

object MyTree {
  def size[A](t: MyTree[A]): Int = t match {
    case MyLeaf(_) => 1
    case MyBranch(l, r) => size(l) + size(r)
  }

  def maximum(t: MyTree[Int]): Int = t match {
    case MyLeaf(v) => v
    case MyBranch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: MyTree[A]): Int = t match {
    case MyLeaf(_) => 1
    case MyBranch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: MyTree[A])(f: A => B): MyTree[B] = t match {
    case MyLeaf(a) => MyLeaf(f(a))
    case MyBranch(l, r) => MyBranch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: MyTree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case MyLeaf(a) => f(a)
    case MyBranch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeWithFold[A](t: MyTree[A]): Int = fold(t)(_ => 1)((_, b) => 1 + b)

  def maximumWithFold(t: MyTree[Int]): Int = fold(t)(a => a)(_ max _)

  def depthWithFold[A](t: MyTree[A]): Int = fold(t)(_ => 1)((a, b) => 1 + (a max b))
}