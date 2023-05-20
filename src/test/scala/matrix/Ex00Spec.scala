package matrix

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Semigroup

class Ex00Spec extends AnyFlatSpec with Matchers:
  "Vector.+" should "return the sum of all elements consider their order" in {
    val v1 = Vector.of(2.0, 3.0)
    val v2 = Vector.of(5.0, 7.0)

    val result = v1 + v2
    result shouldBe Vector.of(7.0, 10.0)
  }

  it should "return the sum of all elements consider their order using substruct Semigroup" in {
    val v1 = Vector.of(2.0, 3.0)
    val v2 = Vector.of(5.0, 7.0)

    given Semigroup[Double] = _ - _
    val result              = v1 + v2

    result shouldBe Vector.of(-3.0, -4.0)
  }

  "Vector.*" should "return the product of all elements and scalar consider their order" in {
    val v1 = Vector.of(2.0, 3.0)

    val result = v1 * 2.0
    result shouldBe Vector.of(4.0, 6.0)
  }

  "Matrix.+" should "return the same size matrix with sums of elements" in {
    val m1: Matrix[2, 2, Double] = Matrix {
      Vector.of(
        Vector.of(1.0, 2.0),
        Vector.of(3.0, 4.0),
      )
    }

    val m2: Matrix[2, 2, Double] = Matrix {
      Vector.of(
        Vector.of(7.0, 4.0),
        Vector.of(-2.0, 2.0),
      )
    }

    val result = m1 + m2
    result shouldBe Matrix[2, 2, Double] {
      Vector.of(
        Vector.of(8.0, 6.0),
        Vector.of(1.0, 6.0),
      )
    }
  }

  it should "return the same size matrix with substructions of elements using substruct Semigroup" in {
    val m1: Matrix[2, 2, Double] = Matrix {
      Vector.of(
        Vector.of(1.0, 2.0),
        Vector.of(3.0, 4.0),
      )
    }

    val m2: Matrix[2, 2, Double] = Matrix {
      Vector.of(
        Vector.of(7.0, 4.0),
        Vector.of(-2.0, 2.0),
      )
    }

    given Semigroup[Double] = _ - _
    val result              = m1 + m2

    result shouldBe Matrix[2, 2, Double] {
      Vector.of(
        Vector.of(-6.0, -2.0),
        Vector.of(5.0, 2.0),
      )
    }
  }

  "Matrix.*" should "return the same size matrix with substructions of elements using substruct Semigroup" in {
    val m1: Matrix[2, 2, Double] = Matrix {
      Vector.of(
        Vector.of(1.0, 2.0),
        Vector.of(3.0, 4.0),
      )
    }

    val result = m1 * 2.0
    result shouldBe Matrix[2, 2, Double] {
      Vector.of(
        Vector.of(2.0, 4.0),
        Vector.of(6.0, 8.0),
      )
    }
  }
