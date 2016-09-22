package com.manning.chapter6

import org.scalatest.FunSpec

class TestCandyMachine extends FunSpec {
  def m1 = Machine(locked = true, 100, 0)

  describe("modify") {
    def reset = (i: Int) => i + 42
    println("MODIFY")
    println(State.modifyDOH[Int](_ + 42).run(58))
  }

  describe("get") {
    println("GET")
    println(State.get.run(42))
  }

  describe("set") {
    println("SET")
    println(State.set(42).run(0))
  }
}