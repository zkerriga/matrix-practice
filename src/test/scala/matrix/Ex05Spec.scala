package matrix

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Ex05Spec extends AnyFlatSpec with Matchers:
  "Vector.angleCos" should "return correct cosine for the identical vectors" in {
    val v1 = Vector.of(1.0, 0.0)
    val v2 = Vector.of(1.0, 0.0)

    Vector.angleCos(v1, v2) shouldBe 1.0
  }

  it should "return correct cosine for perpendicular vectors" in {
    val v1 = Vector.of(1.0, 0.0)
    val v2 = Vector.of(0.0, 1.0)

    Vector.angleCos(v1, v2) shouldBe 0.0
  }

  it should "return correct cosine for vectors with angle = 180 degree" in {
    val v1 = Vector.of(-1.0, 1.0)
    val v2 = Vector.of(1.0, -1.0)

    Vector.angleCos(v1, v2) shouldBe -1.0 +- 1e-9
  }

  it should "return correct cosine for similar scaled vectors" in {
    val v1 = Vector.of(2.0, 1.0)
    val v2 = Vector.of(4.0, 2.0)

    Vector.angleCos(v1, v2) shouldBe 1.0 +- 1e-9
  }

  it should "return correct cosine for random vectors" in {
    val v1 = Vector.of(1.0, 2.0, 3.0)
    val v2 = Vector.of(4.0, 5.0, 6.0)

    Vector.angleCos(v1, v2) shouldBe 0.974631846 +- 1e-9
  }
