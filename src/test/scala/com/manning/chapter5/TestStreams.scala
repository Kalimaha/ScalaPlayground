package com.manning.chapter5

import com.manning.chapter5.Streams.{ManningCons, ManningEmpty, ManningStream}
import org.scalatest.FunSpec

class TestStreams extends FunSpec {

  describe("toList") {

    val s = ManningStream(1, 2, 3)

    it("converts a Stream to a List") {
      assert(s.toList == List(1, 2, 3))
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
      val f = (i: Int) => i % 2 == 0
      val s = ManningStream(1, 2, 3, 4, 5)
      assert(s.takeWhile(f).toList == List(2, 4))
    }
  }
}