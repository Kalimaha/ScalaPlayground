package com.manning.chapter6

import org.scalatest.FunSpec

class TestCandyMachine extends FunSpec {

  def run(m: Machine): ((Int, Int), Machine) = ((0, 100 * m.candies), m)
  val sa = State(run)
  val m = Machine(0, 10)

  describe("The state is composed by a pair of Ints (coins and candies) and a Machine.") {
    it("returns the Machine with the given configuration as part of the state") {
      assert(sa.run(m)._2.coins == 0)
      assert(sa.run(m)._2.candies == 10)
    }

    it("coins and candies are altered when the run function is executed") {
      assert(sa.run(m)._1 == (0, 1000))
    }
  }

  describe("flatMap") {
    def f(a: (Int, Int)): State[String, Machine] =
      State.unit(s"There are ${a._1} coins and ${a._2} candies in the machine.")

    it("instead of returning a pair of Int the State contains a String") {
      assert(sa.run(m)._1._1.getClass.getSimpleName == "int")
      assert(sa.flatMap(f).run(m)._1.getClass.getSimpleName == "String")
    }
  }

  describe("map") {
    def f(a: (Int, Int)): String = s"There are ${a._1} coins and ${a._2} candies in the machine."

    it("converts the type of the value of the state") {
      assert(State(run).map(f).run(m)._1.getClass.getSimpleName == "String")
    }
  }

  describe("map2") {
    val sb = State((m: Machine) => ("Hello, world!", m))
    def f(a: (Int, Int), b: String): Double = (a._1 + a._2).toDouble / b.length

    it("maps two different types into a third one") {
      assert(State(run).map2(sb)(f).run(m)._1.getClass.getSimpleName == "double")
    }
  }

  describe("sequence") {
    val as = List(sa)
    it("converts a list of states into a state with a list as a value") {
      assert(State.sequence(as).run(m)._1 == List((0, 1000)))
    }
  }
}