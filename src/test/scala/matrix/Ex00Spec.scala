package matrix

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Ex00Spec extends AnyFlatSpec with Matchers:
  "Vector.+" should "return the sum of all elements consider their order" in {
    val v1 = Vector.of(2, 3)
    val v2 = Vector.of(5, 7)

    (v1 + v2) shouldBe Vector.of(7, 10)
  }

  "Vector.-" should "return the subtraction of all elements consider their order" in {
    val v1 = Vector.of(2, 3)
    val v2 = Vector.of(5, 7)

    (v1 - v2) shouldBe Vector.of(-3, -4)
  }

  "Vector.*" should "return the product of all elements and scalar consider their order" in {
    val v1 = Vector.of(2, 3)

    (v1 * 2) shouldBe Vector.of(4, 6)
  }

  "Matrix.+" should "return the same size matrix with sums of elements" in {
    val m1: Matrix[2, 2, Int] = Matrix {
      Vector.of(
        Vector.of(1, 2),
        Vector.of(3, 4),
      )
    }

    val m2: Matrix[2, 2, Int] = Matrix {
      Vector.of(
        Vector.of(7, 4),
        Vector.of(-2, 2),
      )
    }

    (m1 + m2) shouldBe Matrix[2, 2, Int] {
      Vector.of(
        Vector.of(8, 6),
        Vector.of(1, 6),
      )
    }
  }

  "Matrix.-" should "return the same size matrix with subtraction of elements" in {
    val m1: Matrix[2, 2, Int] = Matrix {
      Vector.of(
        Vector.of(1, 2),
        Vector.of(3, 4),
      )
    }

    val m2: Matrix[2, 2, Int] = Matrix {
      Vector.of(
        Vector.of(7, 4),
        Vector.of(-2, 2),
      )
    }

    (m1 - m2) shouldBe Matrix[2, 2, Int] {
      Vector.of(
        Vector.of(-6, -2),
        Vector.of(5, 2),
      )
    }
  }

  "Matrix.*" should "return the same size matrix with substructions of elements using substruct Semigroup" in {
    val m1: Matrix[2, 2, Int] = Matrix {
      Vector.of(
        Vector.of(1, 2),
        Vector.of(3, 4),
      )
    }

    (m1 * 2) shouldBe Matrix[2, 2, Int] {
      Vector.of(
        Vector.of(2, 4),
        Vector.of(6, 8),
      )
    }
  }
