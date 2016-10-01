package com.manning.part1.chapter3

import org.scalatest.FunSpec
import MyList._

class TestMyList extends FunSpec {
  describe("sum") {
    it("sums a list of integers") {
      assert(sum(MyList(1, 2, 3)) == 6)
    }
  }

  describe("product") {
    it("multiplies a list of doubles") {
      assert(product(MyList(1, 2, 3)) == 6)
    }
  }
}