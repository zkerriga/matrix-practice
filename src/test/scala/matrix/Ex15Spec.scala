package matrix

import matrix.numbers.complex.{*, given}
import org.apache.commons.math3.complex.Complex
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Ex15Spec extends AnyFlatSpec with Matchers:
  val matrix: Matrix[2, 2, Complex] = Matrix {
    Vector.of(
      Vector.of(C(1, 2), C(3, -1)),
      Vector.of(C(0, 4), C(2, 5)),
    )
  }

  "Matrix.rank" should "work with complex numbers" in {
    matrix.rank shouldBe 2
  }

  "Matrix.inverse" should "work with complex numbers" in {
    val expected = Matrix[2, 2, Complex] {
      Vector.of(
        Vector.of(Complex(-13.0 / 51.0, -6.0 / 17.0), Complex(11.0 / 51.0, -7.0 / 51.0)),
        Vector.of(Complex(4.0 / 51.0, 16.0 / 51.0), Complex(-2.0 / 17.0, -7.0 / 51.0)),
      )
    }
    matrix.inverse.fold[Matrix[2, 2, Unit]](fail("Some is expected")) { result =>
      Matrix.map2[2, 2, Complex, Complex, Unit](result, expected) { (resultValue, expectedValue) =>
        resultValue.getReal shouldBe expectedValue.getReal +- 1e-8
        resultValue.getImaginary shouldBe expectedValue.getImaginary +- 1e-8
        ()
      }
    }
  }

  "Matrix.determinant" should "work with complex numbers" in {
    val result = matrix.determinant[Complex]
    result.getReal shouldBe -12.0
    result.getImaginary shouldBe -3.0
  }
