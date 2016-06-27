package com.twitter.scalaschool

import org.scalatest.FunSuite
import com.twitter.scalaschool.Playground._

class TestPlayground extends FunSuite {

  test("Increase by one.") {
    assert(plusOne(0) == 1)
  }

  /**
    * It is possible to partially apply another function
    * through the use of the underscore in order to
    * obtain a new function.
    */
  test("Partial application") {
    val addOne = add(1, _:Int)
    assert(addOne(0) == 1)
  }

  test("Curried functions: multiply two numbers.") {
    assert(multiply(3)(2) == 6)
    val partial = multiply(3) _
    assert(partial(2) == 6)
  }

  test("Definition of a class.") {
    val c = new Calculator
    assert(c.add(2, 2) == 4)
  }

  test("Traits can be used to implement multiple inheritance.") {
    val srv = new SRV
    assert(srv.toString == "Fast Guitar")
  }

  test("Generic types.") {
    assert(print("Hello World") == "Printing Hello World")
    assert(print(2) == "Printing 2")
  }

}
