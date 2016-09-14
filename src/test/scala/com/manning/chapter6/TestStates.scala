package com.manning.chapter6

import org.scalatest.FunSpec
import States._

class TestStates extends FunSpec {

  val rng = SimpleRNG(42)

  describe("nonNegativeInt") {
    it("generates a random number") {
      assert(nonNegativeInt(rng)._1 == 16159453)
    }
  }

  describe("double") {
    it("generates a Double") {
      assert(double(rng)._1.getClass.getSimpleName == "double")
    }

    it("generates a value between 0 and 1") {
      assert(double(rng)._1 < 1)
    }

    it("generates a Double (with map)") {
      assert(doubleMap(rng)._1.getClass.getSimpleName == "double")
    }

    it("generates a value between 0 and 1 (with map)") {
      assert(doubleMap(rng)._1 < 1)
    }
  }

  describe("intDouble") {
    val (i1, d1) = intDouble(rng)._1
    val (i2, d2) = intDoubleBoth(rng)._1

    it("generates a pair containing an Integer and a Double") {
      assert(i1.getClass.getSimpleName == "int")
      assert(d1.getClass.getSimpleName == "double")
    }

    it("generates a pair containing an Integer and a Double (with both)") {
      assert(i2.getClass.getSimpleName == "int")
      assert(d2.getClass.getSimpleName == "double")
    }
  }

  describe("doubleInt") {
    val (d1, i1) = doubleInt(rng)._1
    val (d2, i2) = doubleIntBoth(rng)._1

    it("generates a pair containing a Double and an Integer") {
      assert(i1.getClass.getSimpleName == "int")
      assert(d1.getClass.getSimpleName == "double")
    }

    it("generates a pair containing a Double and an Integer (with both)") {
      assert(i2.getClass.getSimpleName == "int")
      assert(d2.getClass.getSimpleName == "double")
    }
  }

  describe("double3") {
    val (d1, d2, d3) = double3(rng)._1

    it("generates three Double values") {
      assert(d1.getClass.getSimpleName == "double")
      assert(d2.getClass.getSimpleName == "double")
      assert(d3.getClass.getSimpleName == "double")
    }
  }

  describe("ints") {
    val l = ints(3)(rng)._1

    it("generates a list of length 3") {
      assert(l.length == 3)
    }

    it("generates a list of Int") {
      assert(l == List(16159453, 1281479696, 340305901))
    }
  }

  describe("map") {
    val s_int = unit(42)
    val s_str1 = map(s_int)(_.toString)
    val s_str2 = map_v2(s_int)(_.toString)

    it("maps a type into another") {
      assert(s_str1(rng)._1 == "42")
    }

    it("maps a type into another (with flatMap)") {
      assert(s_str2(rng)._1 == "42")
    }
  }

  describe("map2") {
    val i = unit(42)
    val d = unit(12.3)
    val f = (i: Int, d: Double) => s"The int is $i and the double is $d"

    it("maps two types into a third one") {
      assert(map2(i, d)(f)(rng)._1 == "The int is 42 and the double is 12.3")
    }

    it("maps two types into a third one (with flatMap)") {
      assert(map2_v2(i, d)(f)(rng)._1 == "The int is 42 and the double is 12.3")
    }
  }

  describe("sequence") {
    val i1 = unit(42)
    val i2 = unit(5)
    val i3 = unit(20)
    val is = List(i1, i2, i3)

    assert(sequence(is)(rng)._1 == List(42, 5, 20))
  }

  describe("flatMap") {
    val i = unit(42)
    val f = (i: Int) => unit(i.toString)

    assert(flatMap(i)(f)(rng)._1 == "42")
  }

  describe("rollDie") {
    it("generates a value bigger than zero") {
      assert(rollDie(rng)._1 > 0)
    }

    it("generates a value smaller than 7") {
      assert(rollDie(rng)._1 < 7)
    }
  }
}