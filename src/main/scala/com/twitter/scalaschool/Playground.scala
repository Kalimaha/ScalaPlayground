package com.twitter.scalaschool

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

}