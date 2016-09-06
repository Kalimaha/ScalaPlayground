package com.manning.chapter4

sealed trait ManningEither[+E, +A]

  case class ManningLeft[+E](value: E) extends ManningEither[E, Nothing] {
//    def map[B](f: A => B): ManningLeft[B] = ManningLeft(f(this.value))
  }

  case class ManningRight[+A](value: A) extends ManningEither[Nothing, A] {
//    def map[B](f: A => B): ManningRight[B] = ManningRight(f(this.value))
  }