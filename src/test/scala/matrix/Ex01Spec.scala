package matrix

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Ex01Spec extends AnyFlatSpec with Matchers:
  "linearCombination" should "return the correct linear combination of identity vectors and coefficients" in {
    val e1 = Vector.of(1, 0, 0)
    val e2 = Vector.of(0, 1, 0)
    val e3 = Vector.of(0, 0, 1)

    val result = Vector.linearCombination(Vector.of(e1, e2, e3), Vector.of(10.0, -2.0, 0.5))
    result shouldBe Vector.of(10.0, -2.0, 0.5)
  }

  it should "return the correct linear combination of random vectors and coefficients" in {
    val v1 = Vector.of(1.0, 2.0, 3.0)
    val v2 = Vector.of(0.0, 10.0, -100.0)

    val result = Vector.linearCombination(Vector.of(v1, v2), Vector.of(10.0, -2.0))
    result shouldBe Vector.of(10.0, 0.0, 230.0)
  }
