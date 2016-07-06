package org.vbatytskyi.list

import org.scalatest.{Matchers, WordSpec}

class ListSpec extends WordSpec with Matchers {

  "A List" when {

    "apply" should {
      "construct an empty list" in {
        val list = List()
        list shouldBe Nil
      }
      "construct a list" in {
        val list = List(1, 2, 3)
        List.size(list) shouldBe 3
      }
    }

    "tail" should {
      "produce IllegalArgumentException on an empty list" in {
        intercept[IllegalArgumentException] {
          List.tail(List())
        }
      }
      "return tail of the list" in {
        List.tail(List(1, 2, 3)) shouldBe List(2, 3)
      }
    }

    "setHead" should {
      "produce IllegalArgumentException on an empty list" in {
        intercept[IllegalArgumentException] {
          List.setHead(List(), "a")
        }
      }
      "return list with changed head" in {
        List.setHead(List("z", "b", "c"), "a") shouldBe List("a", "b", "c")
      }
    }

    "drop" should {
      "produce IllegalArgumentException for negative index" in {
        intercept[IllegalArgumentException] {
          List.drop(List(1, 2), -1)
        }
      }
      "produce IllegalArgumentException for an empty list" in {
        intercept[IllegalArgumentException] {
          List.drop(List(), 1)
        }
      }
      "drop n first values of a list" in {
        List.drop(List(1, 2, 3, 4, 5, 6, 7), 4) shouldBe List(5, 6, 7)
      }
      "drop 0 first values of a list" in {
        List.drop(List(1), 0) shouldBe List(1)
      }
      "drop 0 first values of an empty list" in {
        List.drop(List(), 0) shouldBe List()
      }
    }

    "dropWhile" should {
      "return empty list, for an empty list" in {
        List.dropWhile(List())(_ => true) shouldBe List()
      }
      "remove list prefix while predicate is true" in {
        List.dropWhile(List(1, 2, 3, 4, 5))(_ < 3) shouldBe List(3, 4, 5)
      }
    }

    "append" should {
      "return list itself, when appended with Nil" in {
        List.append(List(1, 2), List()) shouldBe List(1, 2)
      }
      "return list itself, when Nil is appended with it" in {
        List.append(List(), List(1, 2)) shouldBe List(1, 2)
      }
      "return append of two lists" in {
        List.append(List(1, 2), List(3, 4)) shouldBe List(1, 2, 3, 4)
      }
      "return Nil when appending two Nils" in {
        List.append(List(), List()) shouldBe List()
      }

    }

    "init" should {
      "return Nil for Nil list" in {
        List.init(List()) shouldBe Nil
      }
      "return Nil for one element list" in {
        List.init(List(1)) shouldBe Nil
      }
      "return all but the last element for a list" in {
        List.init(List(1, 2, 3, 4, 5)) shouldBe List(1, 2, 3, 4)
      }
    }

    "reverse" should {
      "return Nil for an empty list" in {
        List.reverse(List()) shouldBe List()
      }
      "return one element list" in {
        List.reverse(List(1)) shouldBe List(1)
      }
      "return reverse of the list" in {
        List.reverse(List(1, 2, 3, 4)) shouldBe List(4, 3, 2, 1)
      }
    }

    "foldRight" should {
      "return initial element for Nil" in {
        List.foldRight(List[Int](), 0)(_ + _) shouldBe 0
      }
      "return fold of the elements for a list" in {
        List.foldRight(List(1, 2, 3), "4")(_.toString + _) shouldBe "1234"
      }
    }

    "length" should {
      "return 0 for Nil" in {
        List.length(List()) shouldBe 0
      }
      "return length for a list" in {
        List.length(List(1, 2, 3, 4)) shouldBe 4
      }
    }

    "foldLeft" should {
      "return initial element for an empty list" in {
        List.foldLeft(List[Int](), 0)(_ + _) shouldBe 0
      }
      "return fold of the element for a list" in {
        List.foldLeft(List(1, 2, 3), "0")(_ + _) shouldBe "0123"
      }
    }

    "sum" should {
      "return 0 for an empty list" in {
        List.sum(List[Int]()) shouldBe 0
      }
      "return sum of element for a list" in {
        List.sum(List(1, 2, 3)) shouldBe 6
      }
    }

    "product" should {
      "return 1 for an empty list" in {
        List.product(List[Int]()) shouldBe 1
      }
      "return sum of element for a list" in {
        List.product(List(1, 2, 3, 4)) shouldBe 24
      }
    }

    "foldLeftLength" should {
      "return 0 for Nil" in {
        List.foldLeftLength(List()) shouldBe 0
      }
      "return length for a list" in {
        List.foldLeftLength(List(1, 2, 3, 4)) shouldBe 4
      }
    }

    "foldRightReverse" should {
      "return Nil for an empty list" in {
        List.foldLeftReverse(List()) shouldBe List()
      }
      "return one element list" in {
        List.foldLeftReverse(List(1)) shouldBe List(1)
      }
      "return reverse of the list" in {
        List.foldLeftReverse(List(1, 2, 3, 4)) shouldBe List(4, 3, 2, 1)
      }
    }

    "foldLeftUsingFoldRight" should {
      "return initial element for an empty list" in {
        List.foldLeftUsingFoldRight(List[Int](), 0)(_ + _) shouldBe 0
      }
      "return fold of the element for a list" in {
        List.foldLeftUsingFoldRight(List(1, 2, 3), "0")(_ + _) shouldBe "0123"
      }
    }

    "foldRightUsingFoldLeft" should {
      "return initial element for Nil" in {
        List.foldRightUsingFoldLeft(List[Int](), 0)(_ + _) shouldBe 0
      }
      "return fold of the elements for a list" in {
        List.foldRightUsingFoldLeft(List(1, 2, 3), "4")(_.toString + _) shouldBe "1234"
      }
    }

    "foldLeftAppend" should {
      "return list itself, when appended with Nil" in {
        List.foldLeftAppend(List(1, 2), List()) shouldBe List(1, 2)
      }
      "return list itself, when Nil is appended with it" in {
        List.foldLeftAppend(List(), List(1, 2)) shouldBe List(1, 2)
      }
      "return append of two lists" in {
        List.foldLeftAppend(List(1, 2), List(3, 4)) shouldBe List(1, 2, 3, 4)
      }
      "return Nil when appending two Nils" in {
        List.foldLeftAppend(List(), List()) shouldBe List()
      }

    }

    "appendMultiple" should {
      "return list itself, when appended with Nil" in {
        List.appendMultiple(List(List(1, 2), List())) shouldBe List(1, 2)
      }
      "return list itself, when Nil is appended with it" in {
        List.appendMultiple(List(List(), List(1, 2))) shouldBe List(1, 2)
      }
      "return append of three lists" in {
        List.appendMultiple(List(Nil, List(1, 2), Nil, List(3, 4))) shouldBe List(1, 2, 3, 4)
      }
      "return Nil when appending three Nils" in {
        List.appendMultiple[Int](List(List(), List(), List())) shouldBe List()
      }
    }

    "addOne" should {
      "return Nil for Nil" in {
        List.addOne(List[Int]()) shouldBe List()
      }
      "return incremented list" in {
        List.addOne(List(1, 2, 3)) shouldBe List(2, 3, 4)
      }
    }

    "toString" should {
      "return Nil for Nil" in {
        List.toString(List[Double]()) shouldBe List()
      }
      "return incremented list" in {
        List.toString(List(1.1, 2, 3)) shouldBe List("1.1", "2.0", "3.0")
      }
    }

    "map" should {
      "return Nil for Nil" in {
        List.map(List[Double]())(_ * 2) shouldBe List()
      }
      "return multiplied list" in {
        List.map(List(1.1, 2, 3): List[Double])(_ * 2) shouldBe List(2.2, 4, 6)
      }
    }

    "filter" should {
      "return Nil for Nil" in {
        List.filter(List[Double]())(_ => true) shouldBe List()
      }
      "return the same list for true function" in {
        List.filter(List(1.1, 2, 3): List[Double])(_ => true) shouldBe List(1.1, 2, 3)
      }
      "return Nil for false function" in {
        List.filter(List(1.1, 2, 3): List[Double])(_ => false) shouldBe List()
      }
      "return filtered list" in {
        List.filter(List(1, 10, 2, 15, 3, 16, 4))(_ < 10) shouldBe List(1, 2, 3, 4)
      }
    }

    "flatMap" should {
      "return Nil for Nil" in {
        List.flatMap(List[Double]())(_ => Nil) shouldBe List()
      }
      "return the same list for identity" in {
        List.flatMap(List(1.1, 2, 3))(List(_)) shouldBe List(1.1, 2, 3)
      }
      "return extended for the repetition function" in {
        List.flatMap(List(1, 2, 3))(a => List("" + a, a, a)) shouldBe List("1", 1, 1, "2", 2, 2, "3", 3, 3)
      }
    }

    "filterByFlatMap" should {
      "return Nil for Nil" in {
        List.filterByFlatMap(List[Double]())(_ => true) shouldBe List()
      }
      "return the same list for true function" in {
        List.filterByFlatMap(List(1.1, 2, 3): List[Double])(_ => true) shouldBe List(1.1, 2, 3)
      }
      "return Nil for false function" in {
        List.filterByFlatMap(List(1.1, 2, 3): List[Double])(_ => false) shouldBe List()
      }
      "return filtered list" in {
        List.filterByFlatMap(List(1, 10, 2, 15, 3, 16, 4))(_ < 10) shouldBe List(1, 2, 3, 4)
      }
    }

    "zipAdd" should {
      "return Nil for both Nils" in {
        List.zipAdd(Nil, Nil) shouldBe Nil
      }
      "return first list if the second is Nil" in {
        List.zipAdd(List(1, 2, 3), Nil) shouldBe List(1, 2, 3)
      }
      "return second list if the first is Nil" in {
        List.zipAdd(Nil, List(1, 2, 3)) shouldBe List(1, 2, 3)
      }
      "return sum of corresponding elements and rest of the second list, if the first is shorter" in {
        List.zipAdd(List(1, 2), List(4, 5, 6)) shouldBe List(5, 7, 6)
      }
      "return sum of corresponding elements and rest of the first list, if the second is shorter" in {
        List.zipAdd(List(1, 2, 3), List(4, 5)) shouldBe List(5, 7, 3)
      }
      "return sum of corresponding elements of two lists" in {
        List.zipAdd(List(1, 2, 3), List(4, 5, 6)) shouldBe List(5, 7, 9)
      }
    }
  }
}
