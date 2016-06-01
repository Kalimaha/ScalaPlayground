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

  test("length a list of numbers") {
    assert(length(List(1, 2, 3)) == 3)
  }

}