package com.manning.chapter4

import org.scalatest.FunSpec

class TestManningEithers extends FunSpec {

  describe("map") {
    val r = Right(42)
    val l = Left("I'm an error.")
  }
}