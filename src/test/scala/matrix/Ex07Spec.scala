package matrix

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Ex07Spec extends AnyFlatSpec with Matchers:
  "Matrix.linearMap" should "return the same vector on identity matrix" in {
    val matrix = Matrix.identity[2, Int]
    val vector = Vector.of(4, 2)

    matrix.linearMap(vector) shouldBe vector
  }

  it should "return multiplied vector on diagonal matrix" in {
    val matrix = Matrix.diagonal[2, Int](2)
    val vector = Vector.of(4, 2)

    matrix.linearMap(vector) shouldBe (vector * 2)
  }

  it should "return correct vector on random square matrix" in {
    val matrix = Matrix[2, 2, Int] {
      Vector.of(
        Vector.of(2, -2),
        Vector.of(-2, 2),
      )
    }
    val vector = Vector.of(4, 2)

    matrix.linearMap(vector) shouldBe Vector.of(4, -4)
  }

  it should "return correct vector on random matrix" in {
    val matrix = Matrix[2, 3, Int] {
      Vector.of(
        Vector.of(2, 1, -3),
        Vector.of(0, -1, 4),
      )
    }
    val vector = Vector.of(3, -2, 1)

    matrix.linearMap(vector) shouldBe Vector.of(1, 6)
  }

  "Matrix.x" should "return identity matrix for two identity matrices" in {
    val id = Matrix.identity[2, Int]

    (id x id) shouldBe id
  }

  it should "return the second matrix if first is identity" in {
    val id = Matrix.identity[2, Int]
    val matrix = Matrix[2, 2, Int] {
      Vector.of(
        Vector.of(2, 1),
        Vector.of(4, 2),
      )
    }

    (id x matrix) shouldBe matrix
  }

  it should "return the correct matrix for two random matrices of the same size" in {
    val m1 = Matrix[2, 2, Int] {
      Vector.of(
        Vector.of(3, -5),
        Vector.of(6, 8),
      )
    }
    val m2 = Matrix[2, 2, Int] {
      Vector.of(
        Vector.of(2, 1),
        Vector.of(4, 2),
      )
    }

    (m1 x m2) shouldBe Matrix[2, 2, Int] {
      Vector.of(
        Vector.of(-14, -7),
        Vector.of(44, 22),
      )
    }
  }

  it should "return the correct matrix for two random matrices" in {
    val m1 = Matrix[2, 3, Int] {
      Vector.of(
        Vector.of(1, 2, 3),
        Vector.of(4, 5, 6),
      )
    }
    val m2 = Matrix[3, 1, Int] {
      Vector.of(
        Vector.of(7),
        Vector.of(8),
        Vector.of(9),
      )
    }

    (m1 x m2) shouldBe Matrix[2, 1, Int] {
      Vector.of(
        Vector.of(50),
        Vector.of(122),
      )
    }
  }
