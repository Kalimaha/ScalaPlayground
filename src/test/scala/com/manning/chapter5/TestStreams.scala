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
}