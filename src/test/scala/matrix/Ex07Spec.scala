package matrix

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Ex07Spec extends AnyFlatSpec with Matchers:
  "Matrix.linearMap" should "return the same vector on identity matrix" in {
    val matrix = Matrix.identity[2, Double]
    val vector = Vector.of(4.0, 2.0)

    matrix.linearMap(vector) shouldBe vector
  }

  it should "return multiplied vector on diagonal matrix" in {
    val matrix = Matrix.diagonal[2, Double](2.0)
    val vector = Vector.of(4.0, 2.0)

    matrix.linearMap(vector) shouldBe (vector * 2)
  }

  it should "return correct vector on random square matrix" in {
    val matrix = Matrix[2, 2, Double] {
      Vector.of(
        Vector.of(2.0, -2.0),
        Vector.of(-2.0, 2.0),
      )
    }
    val vector = Vector.of(4.0, 2.0)

    matrix.linearMap(vector) shouldBe Vector.of(4.0, -4.0)
  }

  it should "return correct vector on random matrix" in {
    val matrix = Matrix[3, 2, Double] {
      Vector.of(
        Vector.of(2.0, 1.0, -3.0),
        Vector.of(0.0, -1.0, 4.0),
      )
    }
    val vector = Vector.of(3.0, -2.0, 1.0)

    matrix.linearMap(vector) shouldBe Vector.of(1.0, 6.0)
  }

  "Matrix.x" should "return identity matrix for two identity matrices" in {
    val id = Matrix.identity[2, Double]

    (id x id) shouldBe id
  }

  it should "return the second matrix if first is identity" in {
    val id = Matrix.identity[2, Double]
    val matrix = Matrix[2, 2, Double] {
      Vector.of(
        Vector.of(2.0, 1.0),
        Vector.of(4.0, 2.0),
      )
    }

    (id x matrix) shouldBe matrix
  }

  it should "return the correct matrix for two random matrices of the same size" in {
    val m1 = Matrix[2, 2, Double] {
      Vector.of(
        Vector.of(3.0, -5.0),
        Vector.of(6.0, 8.0),
      )
    }
    val m2 = Matrix[2, 2, Double] {
      Vector.of(
        Vector.of(2.0, 1.0),
        Vector.of(4.0, 2.0),
      )
    }

    (m1 x m2) shouldBe Matrix[2, 2, Double] {
      Vector.of(
        Vector.of(-14.0, -7.0),
        Vector.of(44.0, 22.0),
      )
    }
  }

  it should "return the correct matrix for two random matrices" in {
    val m1 = Matrix[3, 2, Int] {
      Vector.of(
        Vector.of(1, 2, 3),
        Vector.of(4, 5, 6),
      )
    }
    val m2 = Matrix[1, 3, Int] {
      Vector.of(
        Vector.of(7),
        Vector.of(8),
        Vector.of(9),
      )
    }

    (m1 x m2) shouldBe Matrix[1, 2, Int] {
      Vector.of(
        Vector.of(50),
        Vector.of(122),
      )
    }
  }
