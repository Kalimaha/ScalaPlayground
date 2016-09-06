package com.manning.chapter4

sealed trait ManningOption[+A]

  case class ManningSome[+A](get: A) extends ManningOption[A] {
    def getOrElse[B >: A](default: => B): B = this.get
    def map[B](f: A => B): ManningOption[B] = ManningSome(f(this.get))
    def flatMap[B](f: A => ManningOption[B]): ManningOption[B] = f(this.get)
  }

  case object ManningNone extends ManningOption[Nothing] {
    def getOrElse[A, B >: A](default: => B): B = default
    def map[A, B](f: A => B): ManningOption[B] = ManningNone
    def flatMap[A, B](f: A => ManningOption[B]): ManningOption[B] = ManningNone
  }

object ManningOption {
  def orElse[A, B >: A](o: ManningOption[A])(ob: => ManningOption[B]): ManningOption[B] = o match {
    case ManningSome(_) => o
    case ManningNone => ob
  }
}