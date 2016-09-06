package com.manning.chapter4

import org.scalatest.FunSpec

class TestOptions extends FunSpec {

  val o = ManningSome(42)

  describe("getOrElse") {
    it("returns the value of the option, if any") {
      assert((o getOrElse "No values") == 42)
    }

    it("returns the default value, if no value is present") {
      assert((ManningNone getOrElse "No values") == "No values")
    }
  }

  describe("map") {
    val f = (i: Int) => s"Welcome $i!"

    it("transforms an Option of type A into an option of type B") {
      assert((o map f) == ManningSome("Welcome 42!"))
    }

    it("transforms a None into a None") {
      assert((ManningNone map f) == ManningNone)
    }
  }

  describe("flatMap") {
    def f(i: Int): ManningOption[String] = ManningSome(s"Welcome $i!")

    it("maps and then flats") {
      assert((o flatMap f) == ManningSome("Welcome 42!"))
    }

    it("transforms a None into a None") {
      assert((ManningNone flatMap f) == ManningNone)
    }
  }

  describe("orElse") {
    it("returns the options value, if any") {
      assert(ManningOption.orElse(o)(ManningSome(-1)) == ManningSome(42))
    }

    it("returns the default value, if no value is present") {
      assert(ManningOption.orElse(ManningNone)(ManningSome(-1)) == ManningSome(-1))
    }
  }
}