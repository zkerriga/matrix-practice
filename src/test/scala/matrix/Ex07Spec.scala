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

/*
let u = Matrix::from([
[1., 0.],
[0., 1.],
]);
let v = Matrix::from([
[1., 0.],
[0., 1.],
 * */
