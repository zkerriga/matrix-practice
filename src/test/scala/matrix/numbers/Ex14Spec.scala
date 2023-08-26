package matrix.numbers

import matrix.{Matrix, Vector}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Ex14Spec extends AnyFlatSpec with Matchers:
  "Matrix.projection" should "return correct projection matrix" in {
    val result: Matrix[4, 4, Double] =
      Matrix.projection[Double](fov = 60.0, ratio = 1024.0 / 1024.0, near = 1, far = 2)

    val expected = Matrix {
      Vector.of(
        Vector.of(1.7320508075688774, 0.0, 0.0, 0.0),
        Vector.of(0.0, 1.7320508075688774, 0.0, 0.0),
        Vector.of(0.0, 0.0, -3.0, -4.0),
        Vector.of(0.0, 0.0, -1.0, 0.0),
      )
    }
    Matrix.map2(result, expected) { (result, expected) =>
      result shouldBe expected +- 1e-8
      ()
    }
  }
