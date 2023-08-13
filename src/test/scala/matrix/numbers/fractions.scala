package matrix.numbers

import math.aliases.*
import math.syntax.*
import math.{HEq, One, Zero}
import matrix.Matrix
import org.apache.commons.math3.fraction.{BigFraction, Fraction}

object fractions:
  given Div[Fraction]  = (f1: Fraction, f2: Fraction) => f1.divide(f2)
  given Mul[Fraction]  = (f1: Fraction, f2: Fraction) => f1.multiply(f2)
  given Sub[Fraction]  = (f1: Fraction, f2: Fraction) => f1.subtract(f2)
  given Add[Fraction]  = (f1: Fraction, f2: Fraction) => f1.add(f2)
  given Zero[Fraction] = Zero(Fraction.ZERO)
  given One[Fraction]  = One(Fraction.ONE)
  given Eq[Fraction]   = HEq.fromUniversal

  given Div[BigFraction]  = (f1: BigFraction, f2: BigFraction) => f1.divide(f2)
  given Mul[BigFraction]  = (f1: BigFraction, f2: BigFraction) => f1.multiply(f2)
  given Sub[BigFraction]  = (f1: BigFraction, f2: BigFraction) => f1.subtract(f2)
  given Add[BigFraction]  = (f1: BigFraction, f2: BigFraction) => f1.add(f2)
  given Zero[BigFraction] = Zero(BigFraction.ZERO)
  given One[BigFraction]  = One(BigFraction.ONE)
  given Eq[BigFraction]   = HEq.fromUniversal

  extension [H <: Int, W <: Int](matrix: Matrix[H, W, Int])
    def toFraction: Matrix[H, W, Fraction]       = matrix.map(Fraction(_))
    def toBigFraction: Matrix[H, W, BigFraction] = matrix.map(BigFraction(_))

  def F(num: Int, den: Int): Fraction    = Fraction(num, den)
  def F(i: Int): Fraction                = Fraction(i)
  def B(num: Int, den: Int): BigFraction = BigFraction(num, den)
  def B(i: Int): BigFraction             = BigFraction(i)
