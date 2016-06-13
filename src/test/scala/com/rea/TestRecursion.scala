package com.rea

import com.rea.Recursion._
import org.scalatest.FunSuite

class TestRecursion extends FunSuite {

  test("Increase by one.") {
    assert(plusOne(1) == 2)
  }

  test("Decrease by one.") {
    assert(minusOne(1) == 0)
  }

  test("Sum two numbers.") {
    assert(add(2, 3) == 5)
  }

  test("Sum a list of numbers.") {
    assert(sum(List(1, 2, 3)) == 6)
  }

  test("Calculate the length of a list.") {
    assert(length(List(1, 2, 3)) == 3)
  }

  test("Map a list of A's to a list of B's.") {
   assert(map(List(1, 2, 3), (x: Int) => x.toString) == List("1", "2", "3"))
  }

  test("Filter a list.") {
   assert(filter(List(1, 2, 3, 4, 5), (x: Int) => x < 4) == List(1, 2, 3))
  }

  test("Append a list to another.") {
   assert(append(List(1, 2), List(3, 4)) == List(1, 2, 3, 4))
  }

  test("Flatten a list of lists to a single list.") {
   assert(flatten(List(List(1, 2), List(3, 4))) == List(1, 2, 3, 4)) 
  }

  test("Maximum of a list.") {
   assert(maximum(List(1, 2, 3)) == 3)
  }

  test("Reverse a list.") {
   assert(reverse(List(1, 2, 3)) == List(3, 2, 1))
  }

}
