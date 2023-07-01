package matrix

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Ex08Spec extends AnyFlatSpec with Matchers:
  "Matrix.trace" should "return its size for identity matrix" in {
    Matrix.identity[2, Int].trace shouldBe 2
    Matrix.identity[10, Int].trace shouldBe 10
  }

  it should "return the sum of diagonal elements for random matrix" in {
    Matrix[3, 3, Int] {
      Vector.of(
        Vector.of(2, -5, 0),
        Vector.of(4, 3, 7),
        Vector.of(-2, 3, 4),
      )
    }.trace shouldBe 9

    Matrix[3, 3, Int] {
      Vector.of(
        Vector.of(-2, -8, 4),
        Vector.of(1, -23, 4),
        Vector.of(0, 6, 4),
      )
    }.trace shouldBe -21
  }
