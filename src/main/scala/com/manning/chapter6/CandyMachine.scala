package com.manning.chapter6

case class Machine(coins: Int, candies: Int)

sealed trait Action
  case object Coin extends Action
  case object Turn extends Action

case class State[+A, S](run: S => (A, S)) {
  import State._

  def flatMap[B](f: A => State[B, S]): State[B, S] = State(s0 => {
    val (v, s1) = run(s0)
    f(v) run s1
  })

  def map[B](f: A => B): State[B, S] = flatMap(s => unit(f(s)))

  def map2[B, C](sb: State[B, S])(f: (A, B) => C): State[C, S] = flatMap(a => sb.map(b => f(a, b)))
}

object State {
  def unit[S, A](a: A): State[A, S] = State(s => (a, s))
}

object CandyMachine {

}