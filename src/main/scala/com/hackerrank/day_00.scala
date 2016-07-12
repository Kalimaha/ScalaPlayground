package com.hackerrank

/**
  * To complete this challenge, you must save a line of
  * input from stdin to a variable, on a single line,
  * and finally print the value of your
  * variable on a second line.
  */
object day_00 {
  def main(args: Array[String]): Unit = {
    for (ln <- io.Source.stdin.getLines) println("Hello, World.\n" + ln)
  }
}
