package org.vbatytskyi.option

import org.scalatest.{Matchers, WordSpec}

class ListOperationsSpec extends WordSpec with Matchers {

  "List operations" when {
    "variance" should {
      "return None when invoked on an empty seq" in {
        ListOperations.variance(Seq.empty) shouldBe scala.None
      }
      "return Some(0) for a single-element seq" in {
        ListOperations.variance(Seq(1)) shouldBe scala.Some(0)
      }
      "return variance for a seq" in {
        ListOperations.variance(Seq(1, 9)) shouldBe scala.Some(32 / 2)
      }
    }
  }
}
