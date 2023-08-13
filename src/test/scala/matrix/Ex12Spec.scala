package matrix

import math.aliases.*
import math.{HEq, One, Zero}
import matrix.numbers.Fractions.{*, given}
import org.apache.commons.math3.fraction.{BigFraction, Fraction}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Ex12Spec extends AnyFlatSpec with Matchers:
  "Matrix.inverse" should "return identity for identity marix" in {
    val matrix: Matrix[3, 3, Fraction] = Matrix.identity[3, Fraction]

    matrix.inverse shouldBe Some(matrix)
  }

  it should "return correct diagonal matrix for diagonal matrix" in {
    val matrix: Matrix[4, 4, Fraction]   = Matrix.diagonal[4, Fraction](F(2))
    val expected: Matrix[4, 4, Fraction] = Matrix.diagonal[4, Fraction](F(1, 2))

    matrix.inverse shouldBe Some(expected)
  }

  it should "return correct inverted matrix for random 3x3 matrix" in {
    val matrix: Matrix[3, 3, Fraction] = Matrix {
      Vector.of(
        Vector.of(8, 5, -2),
        Vector.of(4, 7, 20),
        Vector.of(7, 6, 1),
      )
    }.toFraction

    matrix.inverse shouldBe Some {
      Matrix[3, 3, Fraction] {
        Vector.of(
          Vector.of(F(113, 174), F(17, 174), F(-19, 29)),
          Vector.of(F(-68, 87), F(-11, 87), F(28, 29)),
          Vector.of(F(25, 174), F(13, 174), F(-6, 29)),
        )
      }
    }
  }

  it should "return None in case of Determinant is zero" in {
    val matrix: Matrix[2, 2, Fraction] = Matrix {
      Vector.of(
        Vector.of(2, 4),
        Vector.of(2, 4),
      )
    }.toFraction

    matrix.inverse shouldBe None
  }
