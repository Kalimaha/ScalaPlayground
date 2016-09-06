package com.manning.chapter4

import org.scalatest.FunSpec

class TestEithersMaths extends FunSpec {

  val l = List(1.0, 2.0, 3.0, 4.0, 5.0)
  val e = List()

  describe("mean") {
    it("computes the mean of a non-empty list") {
      assert(EithersMaths.mean(l) == Right(3))
    }

    it("does not explode for an empty list") {
      assert(EithersMaths.mean(e) == Left("The list provided is empty."))
    }
  }
}