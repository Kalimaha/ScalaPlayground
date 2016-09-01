package com.manning

import org.scalatest.FunSpec
import com.manning.Chapter2._

class TestChapter2 extends FunSpec {

  describe("factorial") {

    it("computes the factorial of a number") {
      assert(factorial(5) == 120)
    }
  }

  describe("findFirst") {

    it("finds a existing string in a collection") {
      assert(findFirst(List("foo", "bar"), "bar") == 1)
    }

    it("flags a non-existing string in a collection") {
      assert(findFirst(List("foo", "bar"), "baz") == -1)
    }
  }

  describe("fibonacci") {

    it("computes the 1st Fibonacci's number") {
      assert(fibonacci(1) == 0)
    }

    it("computes the 2nd Fibonacci's number") {
      assert(fibonacci(2) == 1)
    }

    it("computes the nth Fibonacci's number") {
      assert(fibonacci(8) == 13)
    }
  }

  describe("isSorted") {

    def f(a: Int, b: Int): Boolean = a < b

    it("detects a sorted list") {
      assert(isSorted(List(1, 2, 3), f))
    }

    it("detects a non-sorted list") {
      assert(!isSorted(List(3, 2, 1), f))
    }
  }
}