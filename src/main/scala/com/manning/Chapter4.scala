package com.manning

object Chapter4 {

  sealed trait ManningOption[+A]
    case class ManningSome[+A](get: A) extends ManningOption[A]
    case object ManningNone extends ManningOption[Nothing]

  object ManningOption {

    def map[A, B](o: ManningOption[A])(f: A => B): ManningOption[B] = o match {
      case ManningNone => ManningNone
      case ManningSome(a) => ManningSome(f(a))
    }

    def getOrElse[A, B >: A](o: ManningOption[A])(default: => B): B = o match {
      case ManningNone => default
      case ManningSome(a) => a
    }

    def flatMap[A, B](o: ManningOption[A])(f: A => ManningOption[B]): ManningOption[B] = o match {
      case ManningNone => ManningNone
      case ManningSome(a) => f(a)
    }

    def orElse[A, B >: A](o: ManningOption[A])(ob: => ManningOption[B]): ManningOption[B] = o match {
      case ManningNone => ob
      case ManningSome(_) => o
    }

    def filter[A, B](o: ManningOption[A])(f: A => Boolean): ManningOption[A] = o match {
      case ManningNone => ManningNone
      case ManningSome(a) => if (f(a)) o else ManningNone
    }
  }
}