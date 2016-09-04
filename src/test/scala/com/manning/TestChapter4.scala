package com.manning

import com.manning.Chapter4.{ManningNone, _}
import org.scalatest.FunSpec

class TestChapter4 extends FunSpec {

  describe("ManningOption") {

    val s1 = ManningSome(42)

    describe("map") {
      val actual = s1 map (a => a.toString)

      it("converts the type according to the given function") { assert(actual == ManningSome("42")) }
    }

    describe("getOrElse") {
      val actual1 = s1 getOrElse -1
      val actual2 = ManningNone getOrElse -1

      it("returns the value, if any") { assert(actual1 == 42) }
      it("returns the default value when no value is available") { assert(actual2 == -1) }
    }

    describe("flatMap") {
      val actual = s1 flatMap (a => ManningSome(a.toString))

      it("maps and then flats") { assert(actual == ManningSome("42")) }
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
      val actual1 = s1 filter ((a: Int) => a % 2 == 0)
      val actual2 = s1 filter ((a: Int) => a % 2 != 0)

      it("returns the options if the value does satisfy the condition") { assert(actual1 == s1) }
      it("returns none if the value does NOT satisfy the condition") { assert(actual2 == ManningNone) }
    }
  }
}