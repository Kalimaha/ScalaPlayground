package com.manning.part1.chapter4

sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B]
  def flatMap[B](f: A => MyOption[B]): MyOption[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](o: MyOption[B]): MyOption[B]
  def filter(f: A => Boolean): MyOption[A]
}

case class MySome[+A](get: A) extends MyOption[A] {
  def map[B](f: A => B): MyOption[B] = MySome(f(this.get))

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = f(this.get)

  def getOrElse[B >: A](default: => B): B = this.get

  def orElse[B >: A](o: MyOption[B]): MyOption[B] = MySome(this.get)

  def filter(f: A => Boolean): MyOption[A] = if (f(this.get)) this else MyNone
}

case object MyNone extends MyOption[Nothing] {
  def map[B](f: Nothing => B): MyOption[B] = MyNone

  def flatMap[B](f: Nothing => MyOption[B]): MyOption[B] = MyNone

  def getOrElse[B >: Nothing](default: => B): B = default

  def orElse[B >: Nothing](o: MyOption[B]): MyOption[B] = o

  def filter(f: Nothing => Boolean): MyOption[Nothing] = MyNone
}