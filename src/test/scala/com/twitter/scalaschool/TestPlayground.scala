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

  test("The apply method.") {
    val polite = new SayHalloTo("World")
    assert(polite() == "Hallo, World!")
  }

  test("Objects.") {
    assert(Timer.current == 1)
  }

  test("Pattern matching.") {
    assert(intlSayHello("it") == "Ciao!")
    assert(intlSayHello("es") == "I don't speak es!")
  }

  test("Pattern matching on the type.") {
    assert(typeFinder(1) == "This is an integer")
    assert(typeFinder("1") == "This is a string")
    assert(typeFinder(1.0) == "I don't know this")
  }

  test("Collection examples.") {
    assert(list == List(1, 2, 3))
    assert(set == Set(1, 2, 3))
    assert(tuple._1 == "localhost")
    assert(tuple._2 == 80)
    assert(map("AL") == "Alabama")
  }

  test("Square a collection with map.") {
    assert(timesTwo(List(1, 2, 3)) == List(2, 4, 6))
  }

  test("Filter a collection with filter.") {
    assert(filter(List(1, 2, 3)) == List(2))
  }

  test("Zip two lists in a list of pairs.") {
    assert(zip(List(1, 2, 3), List(1, 2, 3)) == List((1, 1), (2, 2), (3, 3)))
  }

  test("Partition divides a list into two list according to a condition.") {
    assert(partition(List(1, 2, 3, 4, 5)) == (List(1, 2, 3, 4), List(5)))
  }

  test("Find") {
    assert(find(List(1, 5, 2, 5, 3, 5)) == Some(5))
    assert(find(List(1, 3, 7, 11)).isEmpty)
  }

  test("Drop: it removes the first n elements.") {
    assert(drop(List(1, 2, 3, 4)) == List(4))
  }

  test("Drop While: it removes the first element according to a condition.") {
    assert(dropWhile(List(1, 2, 3, 4)) == List(2, 3, 4))
  }

}
