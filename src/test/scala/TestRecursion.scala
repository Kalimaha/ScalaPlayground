import Recursion._
import org.scalatest.FunSuite

class TestRecursion extends FunSuite {

  test("increase by one") {
    assert(plusOne(1) == 2)
  }

  test("decrease by one") {
    assert(minusOne(1) == 0)
  }

  test("add two numbers") {
    assert(add(2, 3) == 5)
  }

  test("sum a list of numbers") {
    assert(sum(List(1, 2, 3)) == 6)
  }

  test("calculate the length a list of numbers") {
    assert(length(List(1, 2, 3)) == 3)
  }

  test("map a list of As to a list of Bs") {
   assert(map(List(1, 2, 3), (x: Int) => x.toString) == List("1", "2", "3"))
  }

  test("filter a list") {
   assert(filter(List(1, 2, 3, 4, 5), (x: Int) => x < 4) == List(1, 2, 3))
  }

  test("append a list to another") {
   assert(append(List(1, 2), List(3, 4)) == List(1, 2, 3, 4))
  }

  test("flatten a list of lists to a singl list") {
   assert(flatten(List(List(1, 2), List(3, 4))) == List(1, 2, 3, 4)) 
  }

  test("maximum of a list") {
   assert(maximum(List(1, 2, 3)) == 3)
  }

  test("reverse a list") {
   assert(reverse(List(1, 2, 3)) == List(3, 2, 1))
  }

}
