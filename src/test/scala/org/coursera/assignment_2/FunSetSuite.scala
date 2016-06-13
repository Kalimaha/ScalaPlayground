package org.coursera.assignment_2

import org.scalatest.FunSuite
import FunSets._


/**
  * This class is a test suite for the methods in object FunSets. To run
  * the test suite, you can either:
  * - run the "test" command in the SBT console
  * - right-click the file in eclipse and chose "Run As" - "JUnit Test"
  */
class FunSetSuite extends FunSuite {

  /**
    * Tests are written using the "test" operator and the "assert" method.
    */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
    * For ScalaTest tests, there exists a special equality operator "===" that
    * can be used inside "assert". If the assertion fails, the two values will
    * be printed in the error message. Otherwise, when using "==", the test
    * error message will only say "assertion failed", without showing the values.
    *
    * Try it out! Change the values so that the assertion fails, and look at the
    * error message.
    */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
    * When writing tests, one would often like to re-use certain values for multiple
    * tests. For instance, we would like to create an Int-set and have multiple test
    * about it.
    *
    * Instead of copy-pasting the code for creating the set into every test, we can
    * store it in the test class using a val:
    *
    * val s1 = singletonSet(1)
    *
    * However, what happens if the method "singletonSet" has a bug and crashes? Then
    * the test methods are not even executed, because creating an instance of the
    * test class fails!
    *
    * Therefore, we put the shared values into a separate trait (traits are like
    * abstract classes), and create an instance inside each test method.
    *
    */
  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
    var setOfNumbersUpToFive = union(s1, s2)
    setOfNumbersUpToFive = union(setOfNumbersUpToFive, s3)
    setOfNumbersUpToFive = union(setOfNumbersUpToFive, s4)
    setOfNumbersUpToFive = union(setOfNumbersUpToFive, s5)
    val setOfEvenNumbers = union(s2, s4)
  }

  /**
    * This test is currently disabled (by using "ignore") because the method
    * "singletonSet" is not yet implemented and the test would fail.
    *
    * Once you finish your implementation of "singletonSet", exchange the
    * function "ignore" by "test".
    */
  test("singletonSet(1) contains 1") {

    /**
      * We create a new instance of the "TestSets" trait, this gives us access
      * to the values "s1" to "s3".
      */
    new TestSets {
      assert(contains(s1, 1), "A singleton set containing only 1")
      assert(!contains(s1, 2), "A singleton set containing only 1 does not contain 2")
      assert(contains(s2, 2), "A singleton set containing only 2")
      assert(contains(s3, 3), "A singleton set containing only 3")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "combined set does contain 1")
      assert(contains(s, 2), "combined set does contain 2")
      assert(!contains(s, 3), "combined set does not contain three")
      assert(!contains(s, 0), "combined set does not contain zero")
      val newSet = union(s1, singletonSet(0))
      assert(contains(newSet, 1), "combined set contains 1")
      assert(contains(newSet, 0), "combined set does contain zero")
      val setMadeUpOfNilAndSingletonSet = union(Nil, s1)
      val setMadeUpOfSingletonSetAndNil = union(s1, Nil)
      contains(setMadeUpOfSingletonSetAndNil, 1)
      intercept[IndexOutOfBoundsException] {
        contains(setMadeUpOfNilAndSingletonSet, 1)
        !contains(setMadeUpOfNilAndSingletonSet, 0)
        !contains(setMadeUpOfSingletonSetAndNil, 0)
      }
    }
  }

  test("intersect contains elements present in both sets") {
    new TestSets {
      val s = intersect(s1, s1)
      assert(contains(s, 1), "element 1 is in both sets")
      assert(!contains(s, 2), "combined set does contain 2")
      val intesectSet = intersect(setOfNumbersUpToFive, setOfEvenNumbers)
      assert(!contains(intesectSet, 1))
      assert(contains(intesectSet, 2))
      assert(!contains(intesectSet, 3))
      assert(contains(intesectSet, 4))
      assert(!contains(intesectSet, 5))
    }
  }

  test("diff gathers the elements which are only in the first set") {
    new TestSets {
      val s = diff(s1, s1)
      assert(!contains(s, 1), "element 1 is in both sets so is not in the combined diff")
      val diffSet = diff(setOfNumbersUpToFive, setOfEvenNumbers)
      assert(contains(diffSet, 1))
      assert(!contains(diffSet, 2))
      assert(contains(diffSet, 3))
      assert(!contains(diffSet, 4))
      assert(contains(diffSet, 5))
    }
  }

  test("filter gathers items in the set that satisfy a predicate") {
    new TestSets {
      val filterValuesLessThan3: Set = filter(setOfNumbersUpToFive, (x: Int) => x < 3)
      assert(contains(filterValuesLessThan3, 1))
      assert(contains(filterValuesLessThan3, 2))
      assert(!contains(filterValuesLessThan3, 3))
      assert(!contains(filterValuesLessThan3, 4))
      assert(!contains(filterValuesLessThan3, 5))
      val filterValuesThatEqual2: Set = filter(setOfNumbersUpToFive, (x: Int) => x == 2)
      assert(!contains(filterValuesThatEqual2, 1))
      assert(contains(filterValuesThatEqual2, 2))
      assert(!contains(filterValuesThatEqual2, 3))
      assert(!contains(filterValuesThatEqual2, 4))
      assert(!contains(filterValuesThatEqual2, 5))
    }
  }

  test("iterate through a set of integers, and checks if all entries match the predicate passed in") {
    new TestSets {
      val areAllEntriesLessThan3 = forall(setOfNumbersUpToFive, (x: Int) => x < 3)
      assert(!areAllEntriesLessThan3, "all items are < 3")
      val areAllEntriesLessThan9 = forall(setOfNumbersUpToFive, (x: Int) => x < 9)
      assert(areAllEntriesLessThan9)
    }
  }

  test("determines whether at least one element in the set satisfies the predicate") {
    new TestSets {
      val isAtLeastOneElementLessThan3 = exists(setOfNumbersUpToFive, (x: Int) => x < 3)
      assert(isAtLeastOneElementLessThan3, "no items are < 3")
      val isAtLeastOneElementEqualTo2 = exists(setOfNumbersUpToFive, (x: Int) => x == 2)
      assert(isAtLeastOneElementEqualTo2, "no items are == 2")
      val isAtLeastOneElementGreaterThan9 = exists(setOfNumbersUpToFive, (x: Int) => x > 9)
      assert(!isAtLeastOneElementGreaterThan9)
      val isThereAtLeastOneElementEqualTo1 = exists(setOfNumbersUpToFive, (x: Int) => x == 1)
      assert(isThereAtLeastOneElementEqualTo1)
      val setOfAllEvenNumbersAnd3ShouldContainAnOddNum = exists(union(setOfEvenNumbers, singletonSet(3)), (x: Int) => x % 2 == 1)
      assert(setOfAllEvenNumbersAnd3ShouldContainAnOddNum, "no odd numbers")
    }
  }

  test("creates a new set by applying a predicate to each element in an existing set") {
    new TestSets {
      val doubleValueSet = map(setOfNumbersUpToFive, (x: Int) => x * 2)
      assert(contains(doubleValueSet, 2))
      assert(contains(doubleValueSet, 4))
      assert(contains(doubleValueSet, 6))
      assert(contains(doubleValueSet, 8))
      assert(contains(doubleValueSet, 10))
      assert(!contains(doubleValueSet, 1))
      assert(!contains(doubleValueSet, 3))
      assert(!contains(doubleValueSet, 5))
    }
  }

}