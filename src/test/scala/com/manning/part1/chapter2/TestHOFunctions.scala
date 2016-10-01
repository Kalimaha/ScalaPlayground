package com.manning.part1.chapter2

import org.scalatest.FunSpec
import HOFunctions._

class TestHOFunctions extends FunSpec {
  describe("fib") {
    it("return the 1st element of the series") {
      assert(fib(1) == 0)
    }

    it("return the 2nd element of the series") {
      assert(fib(2) == 1)
    }

    it("return the 3rd element of the series") {
      assert(fib(3) == 1)
    }

    it("return the nth element of the series") {
      assert(fib(8) == 13)
    }
  }

  describe("isSorted") {
    it("checks that the list IS sorted according to a given comparison function") {
      assert(isSorted(List(1, 2, 3), (a: Int, b: Int) => a < b))
    }

    it("checks that the list is NOT sorted according to a given comparison function") {
      assert(!isSorted(List(3, 2, 1), (a: Int, b: Int) => a < b))
    }
  }

  describe("curry") {
    def f(a: Int, b: Double): String = s"I received $a and $b."
    def c = curry(f)

    it("partially apply a function") {
      assert(c(42)(3.1415) == f(42, 3.1415))
    }
  }

  describe("uncurry") {
    def f(a: Int)(b: Double): String = s"I received $a and $b."
    def c = uncurry(f)

    it("reverses the currying of a function") {
      println(c(42, 3.1415) == f(42)(3.1415))
    }
  }

  describe("compose") {
    def f(b: Double): String = s"I received $b."
    def g(a: Int): Double = 1.0 * a
    it("composes two functions") {
      assert(compose(f, g)(42) == "I received 42.0.")
    }
  }
}