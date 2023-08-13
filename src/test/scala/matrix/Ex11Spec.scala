package matrix

import math.aliases.*
import math.{HEq, One, Zero}
import matrix.core.determinant.LeibnizFormula
import org.apache.commons.math3.fraction.{BigFraction, Fraction}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Ex11Spec extends AnyFlatSpec with Matchers:
  "Matrix.determinant" should "return just value in case of 1x1 matrix" in {
    val matrix0: Matrix[1, 1, Int] = Matrix.diagonal(0)
    matrix0.determinant shouldBe 0

    val matrix5: Matrix[1, 1, Int] = Matrix.diagonal(5)
    matrix5.determinant shouldBe 5
  }

  it should "return correct determinant using LaplaceExpansion with 2x2 matrix" in {
    val matrix1: Matrix[2, 2, Int] = Matrix {
      Vector.of(
        Vector.of(1, -1),
        Vector.of(-1, 1),
      )
    }

    matrix1.determinant shouldBe 0

    val matrix2: Matrix[2, 2, Int] = Matrix {
      Vector.of(
        Vector.of(4, -2),
        Vector.of(3, 6),
      )
    }

    matrix2.determinant shouldBe 30
  }

  it should "return correct determinant using LeibnizFormula with 2x2 matrix" in {
    val matrix1: Matrix[2, 2, Int] = Matrix {
      Vector.of(
        Vector.of(1, -1),
        Vector.of(-1, 1),
      )
    }

    matrix1.determinant(using LeibnizFormula.`2x2`) shouldBe 0

    val matrix2: Matrix[2, 2, Int] = Matrix {
      Vector.of(
        Vector.of(4, -2),
        Vector.of(3, 6),
      )
    }

    matrix2.determinant(using LeibnizFormula.`2x2`) shouldBe 30
  }

  it should "return the multiplication of elements for diagonal 3x3 matrix" in {
    val matrix: Matrix[3, 3, Int] = Matrix.diagonal(2)

    matrix.determinant shouldBe 8
  }

  it should "return correct result for random 3x3 matrix" in {
    val matrix: Matrix[3, 3, Int] = Matrix {
      Vector.of(
        Vector.of(8, 5, -2),
        Vector.of(4, 7, 20),
        Vector.of(7, 6, 1),
      )
    }

    matrix.determinant shouldBe -174
  }

  it should "return correct result for random 4x4 matrix" in {
    val matrix: Matrix[4, 4, Double] = Matrix {
      Vector.of(
        Vector.of(8.0, 5.0, -2.0, 4.0),
        Vector.of(4.0, 2.5, 20.0, 4.0),
        Vector.of(8.0, 5.0, 1.0, 4.0),
        Vector.of(28.0, -4.0, 17.0, 1.0),
      )
    }

    matrix.determinant shouldBe 1032
  }

  it should "return correct result for random 6x6 matrix" in {
    val matrix: Matrix[6, 6, Double] = Matrix {
      Vector.of(
        Vector.of(1.0, 0.5, 50.0, 1.0, -1.0, 13.0),
        Vector.of(5.0, 0.0, 41.0, 9.0, -6.0, 17.0),
        Vector.of(17.0, -2.0, 0.0, -4.0, 3.0, 3.0),
        Vector.of(74.0, 0.0, 8.0, 10.0, -1.0, 8.0),
        Vector.of(3.0, -21.0, 1.0, 0.0, 7.0, 19.0),
        Vector.of(-8.0, -17.0, 21.0, 11.0, 1.0, 0.0),
      )
    }

    matrix.determinant shouldBe -19973913.0 +- 1e-6
  }
