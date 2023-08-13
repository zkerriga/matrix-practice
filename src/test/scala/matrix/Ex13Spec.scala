package matrix

import matrix.numbers.fractions.{*, given}
import org.apache.commons.math3.fraction.{BigFraction, Fraction}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Ex13Spec extends AnyFlatSpec with Matchers:
  "Matrix.rank" should "return size for identity matrix" in {
    val matrix: Matrix[3, 3, Fraction] = Matrix.identity[3, Fraction]
    matrix.rank shouldBe 3
  }

  it should "return deficient rank for 3x4 matrix" in {
    val matrix: Matrix[3, 4, Fraction] = Matrix {
      Vector.of(
        Vector.of(1, 2, 0, 0),
        Vector.of(2, 4, 0, 0),
        Vector.of(-1, 2, 1, 1),
      )
    }.toFraction

    matrix.rank shouldBe 2
  }

  it should "return full rank for 4x3 matrix" in {
    val matrix: Matrix[4, 3, Fraction] = Matrix {
      Vector.of(
        Vector.of(8, 5, -2),
        Vector.of(4, 7, 20),
        Vector.of(7, 6, 1),
        Vector.of(21, 18, 7),
      )
    }.toFraction

    matrix.rank shouldBe 3
  }

  it should "return zero rank for zero matrix" in {
    val matrix: Matrix[2, 2, Fraction] = Matrix {
      Vector.of(
        Vector.of(0, 0),
        Vector.of(0, 0),
      )
    }.toFraction

    matrix.rank shouldBe 0
  }
