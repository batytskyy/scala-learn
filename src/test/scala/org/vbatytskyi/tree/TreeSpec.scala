package org.vbatytskyi.tree

import org.scalatest.{Matchers, WordSpec}

class TreeSpec extends WordSpec with Matchers {

  "A Tree" when {
    "size" should {
      "compute zero" in {
        Tree.size(Nil) shouldBe 0
      }
      "compute 1" in {
        Tree.size(Leaf(100)) shouldBe 1
      }
      "compute 3" in {
        Tree.size(Branch(Leaf(100), Branch(Leaf(5), Nil)))
      }
    }

    "max" should {
      "throw Illegal argument exception for Nil" in {
        intercept[IllegalArgumentException] {
          Tree.max(Nil)
        }
      }
      "return max element for a leaf" in {
        Tree.max(Leaf(-6)) shouldBe -6
      }
      "return max of all elements" in {
        Tree.max(Branch(Branch(Leaf(1), Leaf(2)), Leaf(100))) shouldBe 100
      }
    }

    "depth" should {
      "return zero for a Nil tree" in {
        Tree.depth(Nil) shouldBe 0
      }
      "return 1 for the depth of a leaf" in {
        Tree.depth(Leaf(100)) shouldBe 1
      }
      "return depth of a tree" in {
        Tree.depth(Branch(Nil, Branch(Leaf(1), Branch(Leaf(2), Leaf(100))))) shouldBe 4
      }
    }

    "map" should {
      "return Nil for a Nil tree" in {
        Tree.map(Nil)(_.toString) shouldBe Nil
      }
      "return modified Leaf for a Leaf" in {
        Tree.map(Leaf(1))(_.toString) shouldBe Leaf("1")
      }
      "return modified Tree" in {
        val actualTree = Branch(Branch(Branch(Leaf(5), Nil), Leaf(6)), Nil)
        val expectedTree = Branch(Branch(Branch(Leaf(10), Nil), Leaf(12)), Nil)
        Tree.map(actualTree)(_ * 2) shouldBe expectedTree
      }
    }

    "fold" should {
      "return a neutral element for a Nil tree" in {
        Tree.fold(Nil, 1)(_ => 100) { case v => v._2 } shouldBe 1
      }
      "return a size of a tree" in {
        val actualTree = Branch(Branch(Branch(Leaf(5), Nil), Leaf(6)), Nil)
        Tree.fold(actualTree, 0)(_ => 1)(_ + _ + 1) shouldBe 5
      }
      "return a max element of a tree" in {
        val actualTree = Branch(Branch(Branch(Leaf(5), Nil), Leaf(6)), Nil)
        Tree.fold[Int, Int](actualTree, -10000000)(a => a)(Math.max) shouldBe 6
      }
      "return a depth of a tree" in {
        val actualTree = Branch(Branch(Branch(Leaf(5), Nil), Leaf(6)), Nil)
        Tree.fold(actualTree, 0)(_ => 1) { case (b1, b2) => 1 + Math.max(b1, b2) } shouldBe 4
      }
      "map implemented by a fold" in {
        val actualTree = Branch(Branch(Branch(Leaf(5), Nil), Leaf(6)), Nil)
        val expectedTree = Branch(Branch(Branch(Leaf(10), Nil), Leaf(12)), Nil)
        Tree.fold[Int, Tree[Int]](actualTree, Nil)(a => Leaf(a * 2)) { case (l, r) => Branch(l, r) } shouldBe expectedTree
      }
    }
  }
}
