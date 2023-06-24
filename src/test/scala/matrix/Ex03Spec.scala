package matrix

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Ex03Spec extends AnyFlatSpec with Matchers:
  "Vector.dot" should "return correct dot product of zeros and ones vectors" in {
    val v1 = Vector.of(0, 0)
    val v2 = Vector.of(1, 1)

    (v1 dot v2) shouldBe 0
  }

  it should "return correct dot product of two same vectors" in {
    val v1 = Vector.of(1, 1)
    val v2 = Vector.of(1, 1)

    (v1 dot v2) shouldBe 2
  }

  it should "return correct dot product of two random vectors" in {
    val v1 = Vector.of(-1, 6)
    val v2 = Vector.of(3, 2)

    (v1 dot v2) shouldBe 9
  }
