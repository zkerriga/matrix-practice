package matrix

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Ex06Spec extends AnyFlatSpec with Matchers:
  "Vector.crossProduct" should "return correct cross product vector for simple vectors" in {
    val v1 = Vector.of(0, 0, 1)
    val v2 = Vector.of(1, 0, 0)

    (v1 x v2) shouldBe Vector.of(0, 1, 0)
  }

  it should "return correct cross product vector for two positive vectors" in {
    val v1 = Vector.of(1, 2, 3)
    val v2 = Vector.of(4, 5, 6)

    (v1 x v2) shouldBe Vector.of(-3, 6, -3)
  }

  it should "return correct cross product vector for random vectors" in {
    val v1 = Vector.of(4, 2, -3)
    val v2 = Vector.of(-2, -5, 16)

    (v1 x v2) shouldBe Vector.of(17, -58, -16)
  }
