package matrix

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import math.Interpolation.given

class Ex02Spec extends AnyFlatSpec with Matchers:
  "LinearInterpolation" should "interpolate correct values for Double" in {
    (0.0, 1.0).interpolateBy(0.0) shouldBe 0.0
    (0.0, 1.0).interpolateBy(1.0) shouldBe 1.0
    (0.0, 1.0).interpolateBy(0.5) shouldBe 0.5
    (21.0, 42.0).interpolateBy(0.3) shouldBe 27.3
  }

  it should "interpolate correct Vectors of Double" in {
    val v1 = Vector.of(2.0, 1.0)
    val v2 = Vector.of(4.0, 2.0)

    (v1, v2).interpolateBy(0.3) shouldBe Vector.of(2.6, 1.3)
  }

  it should "interpolate correct Matrices of Double" in {
    val m1 = Matrix[2, 2, Double] {
      Vector.of(
        Vector.of(2.0, 1.0),
        Vector.of(3.0, 4.0),
      )
    }
    val m2 = Matrix[2, 2, Double] {
      Vector.of(
        Vector.of(20.0, 10.0),
        Vector.of(30.0, 40.0),
      )
    }

    (m1, m2).interpolateBy(0.5) shouldBe Matrix[2, 2, Double] {
      Vector.of(
        Vector.of(11.0, 5.5),
        Vector.of(16.5, 22.0),
      )
    }
  }
