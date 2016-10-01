package com.manning.chapter6

sealed trait MachineAction
  case object MachineCoin extends MachineAction
  case object MachineTurn extends MachineAction

case class VendingMachine(locked: Boolean, coins: Int, candies: Int)

case class ComputationStep[S, +V](run: S => (V, S)) {
  def flatMap[W](f: V => ComputationStep[S, W]): ComputationStep[S, W] = ComputationStep({v =>
    val (v1, s1) = run(v)
    f(v1).run(s1)
  })

  def map[W](f: V => W): ComputationStep[S, W] = flatMap(v => ComputationStep.unit(f(v)))

  def map2[W, Z](s: ComputationStep[S, W])(f: (V, W) => Z): ComputationStep[S, Z] =
    flatMap(v => s.map(w => f(v, w)))
}

object ComputationStep {
  def unit[S, V](v: V): ComputationStep[S, V] = ComputationStep(s => (v, s))

  def get[V]: ComputationStep[V, V] = ComputationStep(v => {
    println("[GET]")
    println("\t" + v)
    (v, v)
  })

  def set[V](v: V): ComputationStep[V, Unit] = {
    println("[SET]")
    println("\t" + v)
    ComputationStep(_ => ((), v))
  }

  def modify[S](f: S => S): ComputationStep[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def sequence[S, V](l: List[ComputationStep[S, V]]): ComputationStep[S, List[V]] = {
    println("[SEQUENCE]")
    println("\t" + l)
    def go(s: S, actions: List[ComputationStep[S, V]], acc: List[V]): (List[V], S) =
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
      }
    ComputationStep((s: S) => go(s, l, List()))
  }
}

object CandyMachineSimulator {
  import ComputationStep._

  def main(args: Array[String]) {
    val spark = VendingMachine(locked = true, 0, 10)

    def update: MachineAction => VendingMachine => VendingMachine =
      (a: MachineAction) => (m: VendingMachine) => {
        println("[UPDATE]")
        println("\t" + a)
        println("\t" + m)
        (a, m) match {
          case (MachineCoin, VendingMachine(true, _, _)) =>
            VendingMachine(locked = false, m.coins + 1, m.candies)
          case (MachineTurn, VendingMachine(false, _, _)) =>
            VendingMachine(locked = true, m.coins, m.candies - 1)
        }
      }

    println("[UPDATE DEFINITION]")
    println(update(MachineTurn))
    println(update(MachineTurn)(spark))

    val actions: List[MachineAction] = List(MachineCoin, MachineTurn, MachineCoin, MachineTurn)

    def updateAndModify: MachineAction => ComputationStep[VendingMachine, Unit] = {
      println("[updateAndModify]")
      modify[VendingMachine] _ compose update
    }

    val steps = actions.map(updateAndModify)

//    val steps2 = actions.map(reduce)
//    println("[STEPS2]")
//    println("\t" + steps2)
//    println("\t" + steps2.head(spark))

    println("[STEPS]")
    println("\t" + steps)

    val seq = sequence(steps)

//    print("[SEQUENCE]")
//    println("\t" + seq)
//    println("\t" + seq.run(spark))

//    val seqMap = seq.map(_.toString())
//
//    print("[SEQUENCE MAP]")
//    println("\t" + seqMap)
//    println("\t" + seqMap.run(spark))

//    val sf: ComputationStep[VendingMachine, (Int, Int)] =
//      sequence(steps).flatMap(
//        (pippo: List[Unit]) => {
//          println("[FLATMAP]")
//          println(pippo)
//          get.map(
//            vendingMachine => {
//              println("[FINAL MAP]")
//              println("\t" + vendingMachine)
//              (vendingMachine.coins, vendingMachine.candies)
//            }
//          )
//        }
//      )

//    println(sequence(steps).run(VendingMachine(locked = true, 0, 10)))
//    val ((coins, candies), machine) = sf.run(VendingMachine(locked = true, 0, 10))
//    println("<== ==>")
//    println(sf2.run(VendingMachine(locked = true, 0, 10)))
//    println(coins)
//    println(candies)
//    println(machine)
  }
}