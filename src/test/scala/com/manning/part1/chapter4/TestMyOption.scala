package com.manning.part1.chapter4

import org.scalatest.FunSpec

class TestMyOption extends FunSpec {
  describe("map") {
    it("maps a type into another") {
      assert((MySome(42) map(_.toString)) == MySome("42"))
    }

    it("maps nothing into nothing") {
      assert((MyNone map(_.toString)) == MyNone)
    }
  }
}