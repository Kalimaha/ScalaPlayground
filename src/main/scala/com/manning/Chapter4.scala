package com.manning

object Chapter4 {

  sealed trait ManningOption[+A]

    case class ManningSome[+A](get: A) extends ManningOption[A] {
      def getOrElse[B >: A](default: => B): B = this.get
      def map[B](f: A => B): ManningOption[B] = ManningSome(f(this.get))
      def flatMap[B](f: A => ManningOption[B]): ManningOption[B] = f(this.get)
      def filter[B](f: A => Boolean): ManningOption[A] = if (f(this.get)) this else ManningNone
    }

    case object ManningNone extends ManningOption[Nothing] {
      def getOrElse[A, B >: A](default: => B): B = default
      def map[A, B](f: A => B): ManningOption[B] = ManningNone
      def flatMap[A, B](f: A => ManningOption[B]): ManningOption[B] = ManningNone
      def filter[A, B](o: ManningOption[A])(f: A => Boolean): ManningOption[A] = ManningNone
    }

  object ManningOption {

    def orElse[A, B >: A](o: ManningOption[A])(ob: => ManningOption[B]): ManningOption[B] = o match {
      case ManningNone => ob
      case ManningSome(_) => o
    }
  }

  case class Employee(name: String, role: String, manager: String)

  def find(name: String): Option[Employee] = name match {
    case "jeff" => Some(Employee("Jeff Bezos", "CEO", null))
    case "john" => Some(Employee("John Doe", "Software Developer", "Jeff"))
    case _ => None
  }

  def mean(l: List[Double]): Double = l.sum / l.size
}