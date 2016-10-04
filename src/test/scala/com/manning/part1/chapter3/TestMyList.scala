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

  describe("tail") {
    it("returns the tail of a list") {
      assert(tail(MyList(1, 2, 3)) == MyList(2, 3))
    }

    it("returns MyNil for an empty list") {
      assert(tail(MyList()) == MyNil)
    }

    it("returns the tail of a list (with drop)") {
      assert(tailWithDrop(MyList(1, 2, 3)) == MyList(2, 3))
    }

    it("returns MyNil for an empty list (with drop)") {
      assert(tailWithDrop(MyList()) == MyNil)
    }
  }

  describe("setHead") {
    it("set the head of the list to the given value") {
      assert(setHead(MyList(1, 2, 3))(4) == MyList(4, 2, 3))
    }
  }

  describe("drop") {
    it("removes the first n elements of a list") {
      assert(drop(MyList(1, 2, 3, 4, 5))(3) == MyList(4, 5))
    }
  }

  describe("append") {
    it("appends an element to a list") {
      assert(append(MyList(1, 2, 3))(MyList(4, 5, 6)) == MyList(1, 2, 3, 4, 5, 6))
    }
  }

  describe("dropWhile") {
    it("removes the elements that match a predicate from the list") {
      assert(dropWhile((i: Int) => i % 2 == 0)(MyList(1, 2, 3, 4, 5, 6)) == MyList(1, 3, 5))
    }
  }

  describe("init") {
    it("removes the last element of the list") {
      assert(init(MyList(1, 2, 3)) == MyList(1, 2))
    }
  }
}