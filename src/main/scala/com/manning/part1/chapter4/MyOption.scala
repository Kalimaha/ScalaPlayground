package com.manning.part1.chapter4

sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B]
}

case class MySome[+A](get: A) extends MyOption[A] {
  def map[B](f: (A) => B): MyOption[B] = MySome(f(this.get))
}

case object MyNone extends MyOption[Nothing] {
  def map[B](f: (Nothing) => B): MyOption[B] = MyNone
}