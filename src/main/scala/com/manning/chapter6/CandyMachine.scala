package com.manning.chapter6

case class Machine(locked: Boolean, coins: Int, candies: Int)

sealed trait Action
  case object Coin extends Action
  case object Turn extends Action

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
      }
    State((s: S) => go(s,sas,List()))
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def modifyDOH[Banana](f: Banana => Banana): State[Banana, Unit] =
    get.flatMap(s => {
      println(s"s DENTRO FLATMAP DI GET: $s")
      println(s"s ADESSO PASSO A SET: ${f(s)}")
      set(f(s))
    })

  def get[S]: State[S, S] = State(s => (s, s))

  def getDOH[Banana]: State[Banana, Banana] = State(b => (b, b))

  def set[S](s: S): State[S, Unit] = {
    println(s"SET CHIAMATO DOPO IL GET $s")
    State(_ => ((), s))
  }
}

object CandyMachine {
  import State._

  def update = (i: Action) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(locked = false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(locked = true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Action]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)

  def simulateMachineDOH(inputs: List[Action]): State[Machine, (Int, Int)] =
    sequence(inputs map (modify[Machine] _ compose update)).flatMap(_ => get.map(s => (s.coins, s.candies)))

  def main(args: Array[String]) {
    println(simulateMachine(List(Coin, Turn, Coin, Coin)).run(Machine(locked = true, 10, 0)))
    println(simulateMachineDOH(List(Coin, Turn, Coin, Coin)).run(Machine(locked = true, 10, 0)))
  }
}