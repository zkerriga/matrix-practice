package matrix

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Ex09Spec extends AnyFlatSpec with Matchers:
  "Matrix.transpose" should "return the mirrored matrix" in {
    Matrix[2, 3, Int] {
      Vector.of(
        Vector.of(1, 2, 3),
        Vector.of(4, 5, 6),
      )
    }.transpose shouldBe Matrix[3, 2, Int] {
      Vector.of(
        Vector.of(1, 4),
        Vector.of(2, 5),
        Vector.of(3, 6),
      )
    }
  }
