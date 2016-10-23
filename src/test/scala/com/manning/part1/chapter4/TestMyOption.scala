package com.manning.part1.chapter4

import org.scalatest.FunSpec

class TestMyOption extends FunSpec {
  describe("map") {
    it("maps a type into another") {
      assert((MySome(42) map (_.toString)) == MySome("42"))
    }

    it("maps nothing into nothing") {
      assert((MyNone map (_.toString)) == MyNone)
    }
  }

  describe("flatMap") {
    it("maps and then flats a value") {
      assert((MySome(42) flatMap ((i: Int) => MySome(i.toString))) == MySome("42"))
    }

    it("maps and then flats nothing") {
      assert((MyNone flatMap ((i: Int) => MySome(i.toString))) == MyNone)
    }
  }

  describe("getOrElse") {
    it("returns a value, if any") {
      assert((MySome(42) getOrElse 0) == 42)
    }

    it("returns the default value") {
      assert((MyNone getOrElse 0) == 0)
    }
  }

  describe("orElse") {
    it("returns a value, if any") {
      assert((MySome(42) orElse MySome(24)) == MySome(42))
    }

    it("returns the default value") {
      assert((MyNone orElse MySome(24)) == MySome(24))
    }
  }

  describe("filter") {
    it("returns the option if it does satisfy the predicate") {
      assert((MySome(42) filter (_ % 2 == 0)) == MySome(42))
    }

    it("returns nothing if it does not satisfy the predicate") {
      assert((MySome(42) filter (_ % 2 != 0)) == MyNone)
    }
  }
}