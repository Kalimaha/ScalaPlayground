package com.manning.chapter4

import org.scalatest.FunSpec

class TestOptionsMaths extends FunSpec {

  val l = List(1.0, 2.0, 3.0, 4.0, 5.0)
  val e = List()

  describe("mean") {
    it("computes the mean of a non-empty list") {
      assert(OptionsMaths.mean(l) == Some(3))
    }

    it("does not explode for an empty list") {
      assert(OptionsMaths.mean(e).isEmpty)
    }
  }

  describe("variance") {
    it("computes the variance of a non-empty list") {
      assert(OptionsMaths.variance(l) == Some(2.0))
    }

    it("does not explode for an empty list") {
      assert(OptionsMaths.variance(e).isEmpty)
    }
  }
}