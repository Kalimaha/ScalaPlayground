package com.manning

import com.manning.Chapter4.{ManningNone, _}
import org.scalatest.FunSpec

class TestChapter4 extends FunSpec {

  describe("Some math") {

    describe("mean") {

      it("computes the average") {
        assert(mean(List(1.0, 2.0, 3.0)) == Some(2.0))
      }
    }

    describe("variance") {

      it("computes the variance") {
        assert(variance(List(1, 2, 3)) == Some(0.6666666666666666))
      }
    }
  }

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

  describe("Employee") {

    it("print") {
      val jeff = find("jeff")
      println("Jeff's role is: " + jeff.map(_.role).getOrElse("Default Role"))
      val john = find("john")
      println("===")
      println(john.map(_.manager))
      println("John's manager TYPE is: " + john.flatMap(_.manager))
      println("John's manager is: " + john.flatMap(_.manager).map(_.name).getOrElse("Default Name"))
      println("John's manager role is: " + john.flatMap(_.manager).map(_.role).getOrElse("Default Role"))
    }
  }

  describe("map2") {
    val a = Some(2.0)
    val b = Some(4)
    def f(d: Double, i: Int): String = s"Hello $d and $i!"
    assert(map2(a, b)(f) == Some("Hello 2.0 and 4!"))
  }

  describe("sequence") {
    val l = List(Some(1), Some(2), Some(3))
    assert(sequence(l) == Some(List(1, 2, 3)))

    val s = Some(42)
    def f(i: Int): Option[String] = Some(s"Hello $i!")
    println(s.flatMap(f).map(s => s.length))

    // a flatMap (aa => b map(bb => f(aa, bb)))
    println(s flatMap f map(s => s.length) getOrElse "Boom!" )
  }

  describe("boh") {
    val pippo = List(1,2,3).flatMap { x =>
      List(4,5,6).map { y =>
        x*y
      }
    }
    println("===")
    println(pippo)

    val pluto = Some(2).flatMap { x =>
      Some(3).map { y =>
        x*y
      }
    }
    println("===")
    println(pluto)
    println("OPTION can be considered as a LIST of one element!!")
  }
}
