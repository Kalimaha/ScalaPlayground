package com.manning

import com.manning.Chapter3._
import org.scalatest.FunSpec

class TestChapter3 extends FunSpec {

  describe("ManningTree") {

    val t1 = Branch(Branch(Leaf(10), Leaf(15)), Branch(Leaf(21), Leaf(45)))
    val t2 = Branch(Branch(Leaf(100), Leaf(150)), Branch(Leaf(210), Leaf(450)))

    describe("size") {

      it("counts the nodes of a tree") {
        assert(ManningTree.size(t1) == 7)
      }
    }

    describe("maximum") {

      it("returns the maximum value of a tree") {
        assert(ManningTree.maximum(t1) == 45)
      }
    }

    describe("depth") {

      it("calculates the maximum depth from the root to any leaf") {
        assert(ManningTree.depth(t1) == 2)
      }
    }

    describe("map") {

      it("modifies each element in the tree according to the given function") {
        assert(ManningTree.map(t1)((n: Int) => n * 10) == t2)
      }
    }
  }

  describe("ManningList") {

    val l1 = ManningList(1, 2, 3)
    val l2 = ManningList(4, 5, 6)

    describe("tail") {

      it("removes the first element of a list") {
        assert(ManningList.tail(l1) == ManningList(2, 3))
      }
    }

    describe("setHead") {

      it("replaces the first element of a list with the given value") {
        assert(ManningList.setHead(l1, 0) == ManningList(0, 2, 3))
      }
    }

    describe("drop") {

      it("removes the first n elements of a list") {
        assert(ManningList.drop(l1, 2) == ManningList(3))
      }
    }

    describe("append") {

      it("joins two lists") {
        assert(ManningList.append(l1, l2) == ManningList(1, 2, 3, 4, 5, 6))
      }
    }

    describe("dropWhile") {

      it("removes the first n elements of a list") {
        assert(ManningList.dropWhile(l1, (n: Int) => n % 2 == 0) == ManningList(2))
      }
    }

    describe("foldRight") {

      it("reduces a list to a single value") {
        assert(ManningList.foldRight(l1, 0)((current: Int, total: Int) => total + current) == 6)
      }
    }

    describe("foldLeft") {

      it("reduces a list to a single value") {
        assert(ManningList.foldLeft(l1, 0)((total: Int, current: Int) => total + current) == 6)
      }
    }

    describe("functions implemented with foldRight") {

      it("length") {
        assert(ManningList.length(l1) == 3)
      }

      it("sum") {
        assert(ManningList.sum(l1) == 6)
      }

      it("product") {
        assert(ManningList.product(l2) == 120)
      }
    }

    describe("concat") {

      it("reduces a list of lists into a single list") {
        assert(ManningList.concat(ManningList(l1, l2)) == ManningList(1, 2, 3, 4, 5, 6))
      }
    }

    describe("addOne") {

      it("returns a new list of Integersby adding one to each member") {
        assert(ManningList.addOne(l1) == ManningList(2, 3, 4))
      }
    }

    describe("toString") {

      it("converts a list of Double into Strings") {
        assert(ManningList.toString(ManningList(1.0, 2.0, 3.0)) == ManningList("1.0", "2.0", "3.0"))
      }
    }

    describe("map") {

      it("modifies each element of the list according to a given function") {
        assert(ManningList.map(l1)((n: Int) => n * 10) == ManningList(10, 20, 30))
      }
    }

    describe("filter") {

      it("removes the first n elements of a list") {
        assert(ManningList.filter(l1)((n: Int) => n % 2 != 0) == ManningList(1, 3))
      }

      it("can be implemented with flatMap.") {
        assert(ManningList.filterFlatMap(l1)((n: Int) => n % 2 != 0) == ManningList(1, 3))
      }
    }

    describe("flatMap") {

      it("applies a function to each item of a list of lists") {
        assert(ManningList.flatMap(l1)(i => ManningList(i, i)) == ManningList(1, 1, 2, 2, 3, 3))
      }
    }

    describe("addPairWise") {

      it("adds the corresponding elements of two lists") {
        assert(ManningList.addPairWise(l1, l2) == ManningList(5, 7, 9))
      }
    }

    describe("zipWith") {

      it("adds the corresponding elements of two lists") {
        assert(ManningList.zipWith(l1, l2)((a: Int, b: Int) => a + b) == ManningList(5, 7, 9))
      }

      it("constructs codes") {
        val numbers = ManningList(12, 24)
        val letter = ManningList("Alpha", "Beta")
        val expected = ManningList("Alpha-12", "Beta-24")
        def f(n: Int, l: String): String = l + "-" + n
        assert(ManningList.zipWith(numbers, letter)(f) == expected)
      }
    }
  }
}