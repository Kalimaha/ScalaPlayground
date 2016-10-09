package com.manning.part1.chapter3

import org.scalatest.FunSpec
import MyList._

class TestMyList extends FunSpec {
  describe("sum") {
    it("sums a list of integers") {
      assert(sum(MyList(1, 2, 3)) == 6)
    }
  }

  describe("product") {
    it("multiplies a list of doubles") {
      assert(product(MyList(1, 2, 3)) == 6)
    }
  }

  describe("tail") {
    it("returns the tail of a list") {
      assert(tail(MyList(1, 2, 3)) == MyList(2, 3))
    }

    it("returns MyNil for an empty list") {
      assert(tail(MyList()) == MyNil)
    }

    it("returns the tail of a list (with drop)") {
      assert(tailWithDrop(MyList(1, 2, 3)) == MyList(2, 3))
    }

    it("returns MyNil for an empty list (with drop)") {
      assert(tailWithDrop(MyList()) == MyNil)
    }
  }

  describe("setHead") {
    it("set the head of the list to the given value") {
      assert(setHead(MyList(1, 2, 3))(4) == MyList(4, 2, 3))
    }
  }

  describe("drop") {
    it("removes the first n elements of a list") {
      assert(drop(MyList(1, 2, 3, 4, 5))(3) == MyList(4, 5))
    }
  }

  describe("append") {
    it("appends an element to a list") {
      assert(append(MyList(1, 2, 3), MyList(4, 5, 6)) == MyList(1, 2, 3, 4, 5, 6))
    }

    it("appends an element to a list (with foldRight)") {
      assert(appendWithFoldRight(MyList(1, 2, 3))(MyList(4, 5, 6)) == MyList(1, 2, 3, 4, 5, 6))
    }
  }

  describe("dropWhile") {
    it("removes the elements that match a predicate from the list") {
      assert(dropWhile((i: Int) => i % 2 == 0)(MyList(1, 2, 3, 4, 5, 6)) == MyList(1, 3, 5))
    }
  }

  describe("init") {
    it("removes the last element of the list") {
      assert(init(MyList(1, 2, 3)) == MyList(1, 2))
    }
  }

  describe("foldLeft") {
    it("collapses a list to a single value") {
      assert(foldLeft(MyList(1, 2, 3, 4, 5))(0)(_ + _) ==  15)
      assert(foldLeft(MyList(1, 2, 3, 4, 5))(1)(_ * _) == 120)
    }
  }

  describe("foldRight") {
    it("collapses a list to a single value") {
      assert(foldRight(MyList(1, 2, 3, 4, 5), 0)(_ + _) ==  15)
      assert(foldRight(MyList(1, 2, 3, 4, 5), 1)(_ * _) == 120)
    }
  }

  describe("length") {
    it("computes the length of a list") {
      assert(length(MyList(1, 2, 3)) == 3)
    }
  }

  describe("addOne") {
    it("add one to each element of a list") {
      assert(addOne(MyList(1, 2, 3)) == MyList(2, 3, 4))
    }
  }

  describe("double2String") {
    it("converts a list of doubles into alist of strings") {
      assert(double2String(MyList(1.0, 2.0, 3.0)) == MyList("1.0", "2.0", "3.0"))
    }
  }

  describe("map") {
    it("maps all the elements of a list into another type") {
      assert(map(MyList(1, 2, 3))(_.toString) == MyList("1", "2", "3"))
    }
  }

  describe("filter") {
    it("filters out odd numbers") {
      assert(filter(MyList(1, 2, 3))(_ % 2 != 0) == MyList(1, 3))
    }

    it("filters out odd numbers (with flatMap)") {
      assert(filterWithFlatMap(MyList(1, 2, 3))(_ % 2 != 0) == MyList(1, 3))
    }
  }

  describe("flatMap") {
    it("flat and then maps") {
      assert(flatMap(MyList(1, 2, 3))((a: Int) => MyList(a.toString)) == MyList("1", "2", "3"))
    }
  }

  describe("addPairwise") {
    it("sums the corresponding elements of two lists") {
      assert(addPairwise(MyList(1, 2, 3), MyList(4, 5, 6)) == MyList(5, 7, 9))
    }
  }

  describe("zipWith") {
    it("combines the corresponding elements of two lists") {
      assert(zipWith(MyList(1, 2, 3), MyList("4", "5", "6"))((i: Int, s: String) => 1.0 * i / s.toDouble) == MyList(0.25, 0.4, 0.5))
    }
  }
}