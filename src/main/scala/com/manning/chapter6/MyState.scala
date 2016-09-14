package com.manning.chapter6

case class MyState[S, +A](run: S => (A, S)) {
  import MyState._

  def map[B](f: A => B): MyState[S, B] = flatMap(a => unit(f(a)))

  def flatMap[B](f: A => MyState[S, B]): MyState[S, B] = MyState(
    s => {
      val (a, s1) = run(s)
      f(a) run s1
    }
  )
}

object MyState {
  def unit[S, A](a: A): MyState[S, A] = MyState(s => (a, s))
}