package com.manning.part1.chapter3

import org.scalatest.FunSpec
import MyTree._

class TestMyTree extends FunSpec {
  describe("size") {
    it("counts the number of nodes on a tree") {
      assert(size(MyBranch(MyLeaf(1), MyBranch(MyLeaf(2), MyLeaf(3)))) == 3)
    }

    it("counts the number of nodes on a tree (with fold)") {
      assert(sizeWithFold(MyBranch(MyLeaf(1), MyBranch(MyLeaf(2), MyLeaf(3)))) == 3)
    }
  }

  describe("max") {
    it("finds the maximum value in a tree") {
      assert(maximum(MyBranch(MyLeaf(7), MyBranch(MyLeaf(2), MyLeaf(3)))) == 7)
    }

    it("finds the maximum value in a tree (with fold)") {
      assert(maximumWithFold(MyBranch(MyLeaf(7), MyBranch(MyLeaf(2), MyLeaf(3)))) == 7)
    }
  }

  describe("depth") {
    it("calculates the maximum depth of the tree") {
      assert(depth(MyBranch(MyLeaf(7), MyBranch(MyBranch(MyLeaf(2), MyLeaf(2)), MyLeaf(3)))) == 4)
    }

    it("calculates the maximum depth of the tree (with fold)") {
      assert(depthWithFold(MyBranch(MyLeaf(7), MyBranch(MyBranch(MyLeaf(2), MyLeaf(2)), MyLeaf(3)))) == 4)
    }
  }

  describe("map") {
    it("map each element of a tree into another type") {
      assert(map(MyBranch(MyLeaf(7), MyBranch(MyLeaf(2), MyLeaf(3))))(_.toString) == MyBranch(MyLeaf("7"), MyBranch(MyLeaf("2"), MyLeaf("3"))))
    }
  }

  describe("fold") {
    it("collapses a tree into a single value") {
      assert(fold(MyBranch(MyLeaf(7), MyBranch(MyLeaf(2), MyLeaf(3))))(_.toDouble)(_ + _) == 12.0)
    }
  }
}