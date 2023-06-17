package matrix

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Ex06Spec extends AnyFlatSpec with Matchers:
  "Vector.crossProduct" should "return correct cross product vector for simple vectors" in {
    val v1 = Vector.of(0.0, 0.0, 1.0)
    val v2 = Vector.of(1.0, 0.0, 0.0)

    (v1 x v2) shouldBe Vector.of(0.0, 1.0, 0.0)
  }

  it should "return correct cross product vector for two positive vectors" in {
    val v1 = Vector.of(1.0, 2.0, 3.0)
    val v2 = Vector.of(4.0, 5.0, 6.0)

    (v1 x v2) shouldBe Vector.of(-3.0, 6.0, -3.0)
  }

  it should "return correct cross product vector for random vectors" in {
    val v1 = Vector.of(4.0, 2.0, -3.0)
    val v2 = Vector.of(-2.0, -5.0, 16.0)

    (v1 x v2) shouldBe Vector.of(17.0, -58.0, -16.0)
  }
