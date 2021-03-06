package com.twitter.scalaschool

import scala.annotation.tailrec

object Playground {

  /**
    * Functions are defined through the keyword <code>def</code>.
    *
    * @param x  The initial value
    * @return   The result of the sum
    */
  def plusOne(x: Int): Int = x + 1

  /**
    * Add two numbers.
    *
    * @param x  The 1st value
    * @param y  The 2nd value
    * @return   The sum of the two
    */
  def add(x: Int, y: Int): Int = x + y

  /**
    * Curried functions are defined by splitting the arguments,
    * so that you can fill the first arguments immediately
    * and apply the second at a later stage.
    *
    * @param x  The 1st multiplier
    * @param y  The 2nd multiplier
    * @return   The final product
    */
  def multiply(x: Int)(y: Int): Int = x * y

  /**
    * Classes.
    */
  class Calculator {
    def add(x: Int, y: Int): Int = x + y
  }

  trait Rock {
    val instrument = "Guitar"
  }

  trait Blues {
    val tempo = "Fast"
  }

  /**
    * Traits are collections of fields and behaviors that you can extend or mixin to your classes.
    * They can be used to implement multiple inheritance in classes
    */
  class SRV extends Blues with Rock {
    override def toString = tempo + " " + instrument
  }

  /**
    * Functions can be generic and work with parameters of different type.
    *
    * @param text The thing to be printed.
    * @tparam K   The generic type
    * @return     The final string
    */
  def print[K](text: K) = "Printing " + text

  /**
    * The apply method is the default method of a class.
    *
    * @param name The person to be greeted.
    */
  class SayHalloTo(name: String) {
    def apply() = "Hallo, " + name + "!"
  }

  /**
    * Objects are used to hold single instances of a class and
    * are usually used as factories. Objects and classes can have
    * the same name so the object is used as a factory for the
    * class with the same name (companion objects).
    */
  object Timer {
    var count = 0
    def current = {
      count += 1
      count
    }
  }

  /**
    * Pattern matching.
    *
    * @param lang The desired language.
    * @return     The greeting.
    */
  def intlSayHello(lang: String) = lang match {
    case "it" => "Ciao!"
    case "en" => "Hallo!"
    case _ => "I don't speak " + lang + "!"
  }

  /**
    * Pattern matching can be also applied to types.
    *
    * @param myType Any type
    * @tparam K     The generic type
    * @return       Type recognition
    */
  def typeFinder[K](myType: K) = myType match {
    case i: Int => "This is an integer"
    case s: String => "This is a string"
    case _ => "I don't know this"
  }

  /* A list of numbers. */
  val list = List(1, 2, 3)

  /* Sets have no duplicates. */
  val set = Set(1, 1, 2, 2, 3, 3)

  /* A key-value relationship. */
  val tuple = ("localhost", 80)

  /* A map, which is a collection of tuple. */
  val map = Map("AL" -> "Alabama", "AK" -> "Alaska")

  /**
    * Function <code>map</code> applies a given function to
    * all the elements of a collection and returns a new
    * collection.
    *
    * @param l  The input collection
    * @return   The squared collection
    */
  def timesTwo(l: List[Int]): List[Int] = l.map((i: Int) => i * 2)

  /**
    * Filter removes the element from a collection based on a
    * given function and returns a new collection.
    *
    * @param l  The input collection
    * @return   The squared collection
    */
  def filter(l: List[Int]): List[Int] = l.filter((i: Int) => i % 2 == 0)

  /**
    * Zip aggregates two lists in a single list of pairs.
    *
    * @param x  The 1st list
    * @param y  The 2nd list
    * @tparam K The type of the elements in the lists
    * @return   A single list of pairs
    */
  def zip[K](x: List[K], y: List[K]): List[(K, K)] = x.zip(y)

  /**
<<<<<<< 9c368320798f97b35b1f584a21c7a4afc4001931
    * Partition divides a list into two list according to a condition.
    *
    * @param x  The original list
    * @return   The partitioned list
    */
  def partition(x: List[Int]): (List[Int], List[Int]) = x.partition(_ < 5)

  /**
    * Find returns the first occurrence according to a given condition.
    *
    * @param x  The original list
    * @return   The result of the search
    */
  def find(x: List[Int]): Option[Int] = x.find((i: Int) => i == 5)

  /**
    * Drop removes the first n elements.
    *
    * @param x
    * @tparam K
    * @return
    */
  def drop[K](x: List[K]): List[K] = x.drop(3)

  /**
    * DropWhile removes the FIRST element according to a condition.
    *
    * @param x
    * @return
    */
  def dropWhile(x: List[Int]): List[Int] = x.dropWhile((i: Int) => i % 2 != 0)

  def factorial(x: Int): Int = {
    if (x == 0) 1
    else x * factorial(x - 1)
  }

  /**
    * FoldLeft applies a function to each element of a collection
    * and accumulates the results to an initial value.
    *
    * In this example the initial value is 0. At each iteration the
    * function computes the square of the list member and add it
    * to the accumulator.
    *
    * @param l  A list of numbers
    * @return   Sum of the squares
    */
  def foldLeft(l: List[Int]): Int = {
    l.foldLeft(0) { (acc: Int, next: Int) => acc + (next * next) }
  }

  /**
    * Same as FoldRight but going from right to left.
    *
    * @param l  A list of numbers
    * @return   Sum of squares
    */
  def foldRight(l: List[Int]): Int = {
    l.foldLeft(0) { (acc: Int, next: Int) => acc + (next * next) }
  }

  /**
    * Flatten collapses one level. In this example the input is
    * a list of lists, and the result is a list containing
    * the values of all the nested lists.
    *
    * @param l  A list of lists
    * @return   A singe list
    */
  def flatten(l: List[List[Int]]): List[Int] = {
    l.flatten
  }

  def step_1(f: Double): Double = f + 32
  def step_2(f: Double): Double = f / 5
  def step_3(f: Double): Double = f * 9

  /**
    * Compose is used to nest functions. In this example we
    * have: step_1(step_2(step_3(c)))
    */
  val steps123_compose = step_1 _ compose step_2 _ compose step_3 _
  def celsius2fahrenheit_1(c: Double): Double = steps123_compose(c)

  def step_a(i: Int): Int = i * i
  def step_b(i: Int): Double = i.toFloat / 10

  /**
    * AndThen is used to queue functions. In this example
    * we have step_b(step_a(i)). It is like compose but it
    * calls the first function and then the second.
    */
  val and_then = step_a _ andThen step_b _
  def aTenthOfTheSquare(i: Int): Double = and_then(i)

}