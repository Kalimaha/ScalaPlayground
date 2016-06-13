package org.coursera.assignment_1

object Main {

  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def loop(balance: Int, chars: List[Char]): Boolean = chars match {
      case Nil => balance == 0
      case h :: t =>
        if (balance < 0) false
        else if (h == '(') loop(balance + 1, t)
        else if (h == ')') loop(balance - 1, t)
        else loop(balance, t)
    }
    loop(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = coins.sorted match {
    case Nil => 0
    case h :: t =>
      if (money - h == 0) 1
      else if (money - h < 0) 0
      else countChange(money - h, coins) + countChange(money, t)
  }

}
