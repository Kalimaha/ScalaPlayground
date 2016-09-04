package com.manning

import com.manning.Chapter4._
import org.scalatest.FunSpec

class TestChapter4 extends FunSpec {

  describe("ManningOption") {

    val s1 = ManningSome(42)

    describe("map") {

      it("converts the type according to the given function") {
        assert(ManningOption.map(s1)(a => a.toString) == ManningSome("42"))
      }
    }

    describe("getOrElse") {

      it("returns the value, if any") {
        assert(ManningOption.getOrElse(s1)(-1) == 42)
      }

      it("returns the default value when no value is available") {
        assert(ManningOption.getOrElse(ManningNone)(-1) == -1)
      }
    }

    describe("flatMap") {

      it("maps and then flats") {
        assert(ManningOption.flatMap(s1)(a => ManningSome(a.toString)) == ManningSome("42"))
      }
    }

    describe("orElse") {

      it("executes the function if it's None") {
        assert(ManningOption.orElse(ManningNone)(ManningSome("Yo!")) == ManningSome("Yo!"))
      }

      it("returns the Option when there's a value") {
        assert(ManningOption.orElse(s1)(ManningSome(-1)) == s1)
      }
    }

    describe("filter") {

      it("returns the options if the value does satisfy the condition") {
        assert(ManningOption.filter(s1)((a: Int) => a % 2 == 0) == s1)
      }

      it("returns none if the value does NOT satisfy the condition") {
        assert(ManningOption.filter(s1)((a: Int) => a % 2 != 0) == ManningNone)
      }
    }
  }
}