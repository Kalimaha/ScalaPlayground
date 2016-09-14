package com.manning.chapter6

import org.scalatest.FunSpec
import MyState._
import com.manning.chapter6.States.{RNG, SimpleRNG}

class TestMyState extends FunSpec {

  case class Budget(current: Int)

  case class Account(current: Int) {

    def updateAccount(flow: Int): (Int, Account) = {
      val total = current + flow
      (total, Account(total))
    }
  }

  def update(a: Account, flow: Int): (Int, Account) = {
    a.updateAccount(flow)
  }

  def updates(a: Account, flows: List[Int]): List[(Int, Account)] = flows match {
    case h :: t =>
      val (tot, a1) = a.updateAccount(h)
      (tot, a1) :: updates(a1, t)
    case Nil => Nil
  }

  it("updates") {
    val l = List(30, 10, 20)
    val a = Account(0)
    assert(updates(a, l).last._1 == 60)
  }
}