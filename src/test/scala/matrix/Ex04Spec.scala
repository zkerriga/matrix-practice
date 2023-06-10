package matrix

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Ex04Spec extends AnyFlatSpec with Matchers:
  private val v1 = Vector.of(0.0, 0.0, 0.0)
  private val v2 = Vector.of(1.0, 2.0, 3.0)
  private val v3 = Vector.of(-1.0, -2.0)

  "Vector.norm1" should "return the sum of elements" in {
    v1.norm1 shouldBe 0.0
    v2.norm1 shouldBe 6.0
    v3.norm1 shouldBe 3.0
  }

  "Vector.norm" should "return the root square of sum of squares of element" in {
    v1.norm shouldBe 0.0
    v2.norm shouldBe 3.74165738 +- 1e-8
    v3.norm shouldBe 2.236067977 +- 1e-9
  }

  "Vector.normInf" should "return the root square of sum of squares of element" in {
    v1.normInf shouldBe 0.0
    v2.normInf shouldBe 3.0
    v3.normInf shouldBe 2.0
  }
