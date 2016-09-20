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

  def map[B](f: A => B): State[B, S] =
    flatMap(s => unit(f(s)))

  def map2[B, C](sb: State[B, S])(f: (A, B) => C): State[C, S] =
    flatMap(a => sb.map(b => f(a, b)))
}

object State {
  def unit[A, S](a: A): State[A, S] = State(s => (a, s))

  def sequence[A, S](as: List[State[A, S]]): State[List[A], S] =
    as.foldRight(unit[List[A], S](List()))((c, acc) => c.map2(acc)(_ :: _))
}

object CandyMachine {
//  def update = (a: Action) => (m: Machine) => a match {
//    case Coin => Machine(m.coins + 1, m.candies)
//    case Turn => Machine(m.coins, m.candies - 1)
//  }

  case class S(state: (Int, Int), machine: Machine)

  def reduce(a: Action, m: Machine): S = (a, m) match {
    case (Coin, Machine(_, _)) => S((m.coins + 1, m.candies), Machine(m.coins + 1, m.candies))
    case (Turn, Machine(_, _)) => S((m.coins, m.candies + 1), Machine(m.coins, m.candies + 1))
  }

  def simulate(as: List[Action], s0: S): List[S] = as match {
    case Nil => Nil
    case h::t =>
      val s1 = reduce(h, s0.machine)
      s1 :: simulate(t, s1)
  }

  def main(args: Array[String]) {
    val as = List(Coin, Turn)
    val m0 = Machine(0, 10)
    val s0 = S((0, 10), m0)
    val result = simulate(as, s0)
    println(result)
  }
}