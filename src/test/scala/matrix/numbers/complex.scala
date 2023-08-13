package matrix.numbers

import math.aliases.*
import math.{HEq, One, Zero}
import matrix.Matrix
import org.apache.commons.math3.complex.Complex

object complex:
  given Div[Complex]  = (c1: Complex, c2: Complex) => c1.divide(c2)
  given Mul[Complex]  = (c1: Complex, c2: Complex) => c1.multiply(c2)
  given Sub[Complex]  = (c1: Complex, c2: Complex) => c1.subtract(c2)
  given Add[Complex]  = (c1: Complex, c2: Complex) => c1.add(c2)
  given Zero[Complex] = Zero(Complex.ZERO)
  given One[Complex]  = One(Complex.ONE)
  given Eq[Complex]   = HEq.fromUniversal

  def C(real: Int, imaginary: Int): Complex = Complex(real, imaginary)
