package org.vbatytskyi.option

import org.scalatest.{Matchers, WordSpec}

class OptionSpec extends WordSpec with Matchers {

  "An Option" when {
    "map" should {
      "return None when invoked on None" in {
        None.map(_.toString) shouldBe None
      }
      "return Some containing the result of a function application to it's value when invoked on Some" in {
        Some(10).map(_ * 2) shouldBe Some(20)
      }
    }

    "getOrElse" should {
      "return default value when invoked on None" in {
        None.getOrElse(100) shouldBe 100
      }
      "return container's value when invoked on Some" in {
        Some(10).getOrElse(1) shouldBe 10
      }
    }

    "orElse" should {
      "return default Option when invoked on None" in {
        None.orElse(Some(100)) shouldBe Some(100)
      }
      "return the Option when invoked on Some" in {
        Some(1).orElse(None) shouldBe Some(1)
      }
    }

    "filter" should {
      "return None when invoked on None" in {
        None.filter(_ => true) shouldBe None
      }
      "return None when invoked on Some, but predicate isn't satisfied" in {
        Some(-100).filter(_ > 0) shouldBe None
      }
      "return the Option when invoked on Some and predicate is satisfied" in {
        Some(100).filter(_ > 0) shouldBe Some(100)
      }
    }

    "flatMap" should {
      "return None when invoked on None" in {
        None.flatMap(_ => Some(10)) shouldBe None
      }
      "return Option made out of a container's value, when invoked on Some" in {
        Some(10).flatMap(v => Some(v.toString)) shouldBe Some("10")
      }
    }
  }
}
