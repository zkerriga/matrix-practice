package matrix

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.apache.commons.math3.fraction.{BigFraction, Fraction}
import math.aliases.*
import math.{One, Zero, HEq}

class Ex11Spec extends AnyFlatSpec with Matchers:
  given Mul[Fraction] = (f1: Fraction, f2: Fraction) => f1.multiply(f2)
  given Sub[Fraction] = (f1: Fraction, f2: Fraction) => f1.subtract(f2)
  given Add[Fraction] = (f1: Fraction, f2: Fraction) => f1.add(f2)

  extension [H <: Int, W <: Int](matrix: Matrix[H, W, Int])
    def toFraction: Matrix[H, W, Fraction] = matrix.map(Fraction(_))

  "Matrix.determinant" should "return just value in case of 1x1 matrix" in {
    val matrix0: Matrix[1, 1, Int] = Matrix.diagonal(0)
    matrix0.determinant shouldBe 0

    val matrix5: Matrix[1, 1, Int] = Matrix.diagonal(5)
    matrix5.determinant shouldBe 5
  }
