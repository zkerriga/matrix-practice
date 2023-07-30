package matrix

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.apache.commons.math3.fraction.{BigFraction, Fraction}
import math.aliases.*
import math.{One, Zero, HEq}

class Ex10Spec extends AnyFlatSpec with Matchers:
  given Div[Fraction]  = (f1: Fraction, f2: Fraction) => f1.divide(f2)
  given Mul[Fraction]  = (f1: Fraction, f2: Fraction) => f1.multiply(f2)
  given Sub[Fraction]  = (f1: Fraction, f2: Fraction) => f1.subtract(f2)
  given Zero[Fraction] = Zero(Fraction.ZERO)
  given One[Fraction]  = One(Fraction.ONE)
  given Eq[Fraction]   = HEq.fromUniversal

  given Div[BigFraction]  = (f1: BigFraction, f2: BigFraction) => f1.divide(f2)
  given Mul[BigFraction]  = (f1: BigFraction, f2: BigFraction) => f1.multiply(f2)
  given Sub[BigFraction]  = (f1: BigFraction, f2: BigFraction) => f1.subtract(f2)
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

  "Matrix.rowEchelon" should "return zero matrix for zero matrix" in {
    val matrix11: Matrix[1, 1, Fraction] = Matrix {
      Vector.of(
        Vector.of(0)
      )
    }.toFraction
    matrix11.rowEchelon shouldBe matrix11

    val matrix21: Matrix[2, 1, Fraction] = Matrix {
      Vector.of(
        Vector.of(0),
        Vector.of(0),
      )
    }.toFraction
    matrix21.rowEchelon shouldBe matrix21

    val matrix23: Matrix[2, 3, Fraction] = Matrix {
      Vector.of(
        Vector.of(0, 0, 0),
        Vector.of(0, 0, 0),
      )
    }.toFraction
    matrix23.rowEchelon shouldBe matrix23
  }

  it should "return 1 matrix if it contains only one non-zero number" in {
    val matrix1: Matrix[1, 1, Fraction] = Matrix.diagonal(Fraction.ONE)
    matrix1.rowEchelon shouldBe matrix1

    val matrix3: Matrix[1, 1, Fraction] = Matrix.diagonal(Fraction(3))
    matrix3.rowEchelon shouldBe matrix1

    val matrixR: Matrix[1, 1, Fraction] = Matrix.diagonal(Fraction(-3, 7))
    matrix3.rowEchelon shouldBe matrix1
  }

  it should "return the Identity matrix for diagonal matrices" in {
    val matrix44: Matrix[4, 4, Fraction] = Matrix {
      Vector.of(
        Vector.of(2, 0, 0, 0),
        Vector.of(0, 5, 0, 0),
        Vector.of(0, 0, 1, 0),
        Vector.of(0, 0, 0, -9),
      )
    }.toFraction
    matrix44.rowEchelon shouldBe Matrix.identity[4, Fraction]
  }

  it should "return the same matrix if the left part of matrix is Identity" in {
    val matrix34: Matrix[3, 4, Fraction] = Matrix {
      Vector.of(
        Vector.of(1, 0, 0, -4),
        Vector.of(0, 1, 0, 19),
        Vector.of(0, 0, 1, 3),
      )
    }.toFraction
    matrix34.rowEchelon shouldBe matrix34

    val matrix36: Matrix[3, 6, Fraction] = Matrix {
      Vector.of(
        Vector.of(1, 0, 0, -4, 9, 1),
        Vector.of(0, 1, 0, 19, 0, -5),
        Vector.of(0, 0, 1, 3, 4, 1),
      )
    }.toFraction
    matrix36.rowEchelon shouldBe matrix36
  }

  it should "return correct matrix for wiki example" in {
    // wiki: https://en.wikipedia.org/wiki/Gaussian_elimination
    val matrix1: Matrix[3, 4, Fraction] = Matrix {
      Vector.of(
        Vector.of(1, 3, 1, 9),
        Vector.of(1, 1, -1, 1),
        Vector.of(3, 11, 5, 35),
      )
    }.toFraction

    val expected1: Matrix[3, 4, Fraction] = Matrix {
      Vector.of(
        Vector.of(1, 0, -2, -3),
        Vector.of(0, 1, 1, 4),
        Vector.of(0, 0, 0, 0),
      )
    }.toFraction

    matrix1.rowEchelon shouldBe expected1

    // wiki: https://en.wikipedia.org/wiki/Gaussian_elimination#Example_of_the_algorithm
    val matrix2: Matrix[3, 4, Fraction] = Matrix {
      Vector.of(
        Vector.of(2, 1, -1, 8),
        Vector.of(-3, -1, 2, -11),
        Vector.of(-2, 1, 2, -3),
      )
    }.toFraction

    val expected2: Matrix[3, 4, Fraction] = Matrix {
      Vector.of(
        Vector.of(1, 0, 0, 2),
        Vector.of(0, 1, 0, 3),
        Vector.of(0, 0, 1, -1),
      )
    }.toFraction

    matrix2.rowEchelon shouldBe expected2
  }

  it should "return matrix with correctly skipped zero first column" in {
    val matrix1: Matrix[3, 4, Fraction] = Matrix {
      Vector.of(
        Vector.of(0, 2, 3, 4),
        Vector.of(0, 6, 7, 8),
        Vector.of(0, 3, 2, 1),
      )
    }.toFraction

    val expected1: Matrix[3, 4, Fraction] = Matrix {
      Vector.of(
        Vector.of(0, 1, 0, -1),
        Vector.of(0, 0, 1, 2),
        Vector.of(0, 0, 0, 0),
      )
    }.toFraction

    matrix1.rowEchelon shouldBe expected1
  }

  it should "return matrix with correctly processed columns between zeroed ones" in {
    val matrix1: Matrix[3, 9, Fraction] = Matrix {
      Vector.of(
        Vector.of(1, 2, 3, 4, 5, 6, 7, 8, 9),
        Vector.of(0, 0, 0, 1, 2, 3, 4, 5, 6),
        Vector.of(0, 0, 0, 0, 0, 0, 1, 2, 3),
      )
    }.toFraction

    val expected1: Matrix[3, 9, Fraction] = Matrix {
      Vector.of(
        Vector.of(1, 2, 3, 0, -3, -6, 0, 6, 12),
        Vector.of(0, 0, 0, 1, 2, 3, 0, -3, -6),
        Vector.of(0, 0, 0, 0, 0, 0, 1, 2, 3),
      )
    }.toFraction

    matrix1.rowEchelon shouldBe expected1
  }

  it should "return correct matrix for 1 to 110 matrix" in {
    val matrix1: Matrix[10, 11, Fraction] = Matrix {
      Vector.of(
        Vector.of(2, 1, 3, 4, 5, 6, 7, 8, 9, 10, 11),
        Vector.of(12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22),
        Vector.of(23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33),
        Vector.of(34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44),
        Vector.of(45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55),
        Vector.of(56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66),
        Vector.of(67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77),
        Vector.of(78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88),
        Vector.of(89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99),
        Vector.of(100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110),
      )
    }.toFraction

    val expected1: Matrix[10, 11, Fraction] = Matrix {
      Vector.of(
        Vector.of(F(1), F(0), F(0), F(-1, 3), F(-2, 3), F(-1), F(-4, 3), F(-5, 3), F(-2), F(-7, 3), F(-8, 3)),
        Vector.of(F(0), F(1), F(0), F(-1, 3), F(-2, 3), F(-1), F(-4, 3), F(-5, 3), F(-2), F(-7, 3), F(-8, 3)),
        Vector.of(F(0), F(0), F(1), F(5, 3), F(7, 3), F(3), F(11, 3), F(13, 3), F(5), F(17, 3), F(19, 3)),
        Vector.of(F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0)),
        Vector.of(F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0)),
        Vector.of(F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0)),
        Vector.of(F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0)),
        Vector.of(F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0)),
        Vector.of(F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0)),
        Vector.of(F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0), F(0)),
      )
    }

    matrix1.rowEchelon shouldBe expected1
  }

  it should "return correct matrix for random big matrix" in {
    val matrix: Matrix[5, 7, BigFraction] = Matrix {
      Vector.of(
        Vector.of(0, 1, -5, 0, 9, 52, -90),
        Vector.of(14, 5, 91, 1, 51, 51, 36),
        Vector.of(-12, 3, 3, 11, 83, 9, 2),
        Vector.of(3, -5, 0, 0, -1, -15, 3),
        Vector.of(41, -19, 20, 9, 58, 10, -32),
      )
    }.toBigFraction

    val expected: Matrix[5, 7, BigFraction] = Matrix {
      Vector.of(
        Vector.of(B(1), B(0), B(0), B(0), B(0), B(3429493, 1013368), B(-4514069, 1013368)),
        Vector.of(B(0), B(1), B(0), B(0), B(0), B(4268205, 1013368), B(-1857781, 1013368)),
        Vector.of(B(0), B(0), B(1), B(0), B(0), B(-2219033, 1013368), B(4740937, 1013368)),
        Vector.of(B(0), B(0), B(0), B(1), B(0), B(-13643415, 506684), B(24752779, 506684)),
        Vector.of(B(0), B(0), B(0), B(0), B(1), B(2073987, 506684), B(-3646703, 506684)),
      )
    }

    matrix.rowEchelon shouldBe expected
  }

  it should "make as less calculations as posible" in {
    case class Complexity[A](value: A)
    object Complexity:
      var divCounter: Int = 0
      var mulCounter: Int = 0
      var subCounter: Int = 0

      given [A: Eq]: Eq[Complexity[A]]     = (a, b) => a.value == b.value
      given [A: Zero]: Zero[Complexity[A]] = Zero(Complexity(Zero.of[A]))
      given [A: One]: One[Complexity[A]]   = One(Complexity(One.of[A]))
      import math.syntax.*
      given [A: Div]: Div[Complexity[A]] =
        (a1, a2) =>
          divCounter += 1
          Complexity(a1.value / a2.value)
      given [A: Mul]: Mul[Complexity[A]] =
        (a1, a2) =>
          mulCounter += 1
          Complexity(a1.value * a2.value)
      given [A: Sub]: Sub[Complexity[A]] =
        (a1, a2) =>
          subCounter += 1
          Complexity(a1.value - a2.value)

    // wiki: https://en.wikipedia.org/wiki/Gaussian_elimination#Example_of_the_algorithm
    val matrix2: Matrix[3, 4, Complexity[Fraction]] = Matrix {
      Vector.of(
        Vector.of(2, 1, -1, 8),
        Vector.of(-3, -1, 2, -11),
        Vector.of(-2, 1, 2, -3),
      )
    }.toFraction.map(Complexity(_))

    val result = matrix2.rowEchelon

    val N = 3
    // wiki: https://en.wikipedia.org/wiki/Gaussian_elimination#Computational_efficiency
    val theoreticalDiv: Int    = N * (N + 1) / 2                         // 6
    val theoreticalMulSub: Int = (2 * N * N * N + 3 * N * N - 5 * N) / 6 // 11

    Complexity.divCounter shouldBe theoreticalDiv
    Complexity.mulCounter shouldBe theoreticalMulSub
    Complexity.subCounter shouldBe theoreticalMulSub
  }
