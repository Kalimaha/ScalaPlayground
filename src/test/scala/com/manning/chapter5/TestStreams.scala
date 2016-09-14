package com.manning.chapter5

import com.manning.chapter5.Streams.ManningStream
import org.scalatest.FunSpec

class TestStreams extends FunSpec {

  describe("toList") {
    it("converts a Stream to a List") {
      assert(ManningStream(1, 2, 3).toList == List(1, 2, 3))
    }
  }

  describe("take") {
    it("takes the first n elements of a stream") {
      assert(ManningStream(1, 2, 3).take(2).toList == List(1, 2))
    }
  }

  describe("drop") {
    it("skips the first n elements of a stream") {
      assert(ManningStream(1, 2, 3).drop(2).toList == List(3))
    }

    it("returns an empty Streams if you try to drop too much") {
      assert(ManningStream(1, 2, 3).drop(20).toList == List())
    }
  }

  describe("takeWhile") {
    it("returns all the elements that match a given predicate") {
      assert(ManningStream(1, 2, 3, 4, 5).takeWhile(_ % 2 == 0).toList == List(2, 4))
    }

//    it("returns all the elements that match a given predicate (foldRight)") {
//      assert(ManningStream(1, 2, 3, 4, 5).takeWhile2(_ % 2 == 0).toList == List(2, 4))
//    }
  }

  describe("exists") {
    it("asserts whether the Stream contains a given value") {
      assert(ManningStream(1, 2, 3).exists(_ % 2 == 0))
      assert(!ManningStream(1, 2, 3).exists(_ % 5 == 0))
    }

    it("asserts whether the Stream contains a given value (foldRight)") {
      assert(ManningStream(1, 2, 3).exists2(_ % 2 == 0))
      assert(!ManningStream(1, 2, 3).exists2(_ % 5 == 0))
    }
  }

  describe("foldRight") {
    it("calculates the sum of a Stream") {
      assert(ManningStream(1, 2, 3).foldRight(0)(_ + _) == 6)
    }
  }

  describe("forAll") {
    it("checks whether all the elements in the Stream match a given predicate") {
      assert(ManningStream(1, 2, 3).forAll(_ > 0))
      assert(!ManningStream(1, 2, 3).forAll(_ < 0))
    }
  }

  describe("map") {
    it("converts the type of a Stream") {
      assert(ManningStream(1, 2, 3).map(a => a.toString).toList == List("1", "2", "3"))
    }
  }

  describe("filter") {
    it("filters the Stream according to a given predicate") {
      assert(ManningStream(1, 2, 3).filter(_ > 2).toList == List(3))
    }
  }

  describe("append") {
    it("concatenates two streams") {
      assert(ManningStream(1, 2, 3).append(ManningStream(4, 5, 6)).toList == List(1, 2, 3, 4, 5, 6))
    }
  }

  describe("flatMap") {
    def f(a: Int): ManningStream[String] = ManningStream(a.toString)
    it("flatMaps that shit") {
      assert(ManningStream(1, 2, 3).flatMap(f).toList == List("1", "2", "3"))
    }
  }

  describe("head") {
    it("returns the first value of the Stream") {
      assert(ManningStream(1, 2, 3).head == Some(1))
    }
  }

  describe("find") {
    it("finds the first value in the Stream that matches a given predicate") {
      assert(ManningStream(1, 2, 3).find(_ < 3) == Some(1))
    }
  }

  describe("infinite Streams") {
    it("generates a Stream of a given value") {
      assert(Streams.constant(42).take(3).toList == List(42, 42, 42))
    }

    it("generates a Stream starting from n, then n + 1 and so forth") {
      assert(Streams.from(42).take(3).toList == List(42, 43, 44))
    }

    it("generates the Fibonacci Stream") {
      assert(Streams.fibonacci.take(5).toList == List(0, 1, 1, 2, 3))
    }
  }

  describe("unfold") {
    it("unfolds...") {
      assert(Streams.unfold(0)(i => Some(i, i + 1)).take(3).toList == List(0, 1, 2))
    }
  }
}