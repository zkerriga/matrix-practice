package matrix.core.projection

import math.aliases.*
import math.syntax.*
import math.{One, Zero}
import matrix.{Matrix, Vector}

object ProjectionMatrix:
  def from[A: Tan: Div: Mul: Sub: Add: One: Zero](fov: A, ratio: A, near: A, far: A): Matrix[4, 4, A] =
    val zero = Zero.of[A]
    val one  = One.of[A]
    val two  = one + one

    val halfFovTan = (fov / two).tan

    val x = one / (halfFovTan * ratio)
    val y = one / halfFovTan

    val diff = near - far

    val c = (far + near) / diff
    val d = (two * far * near) / diff

    Matrix[4, 4, A] {
      Vector.of(
        Vector.of(x, zero, zero, zero),
        Vector.of(zero, y, zero, zero),
        Vector.of(zero, zero, c, d),
        Vector.of(zero, zero, -one, zero),
      )
    }
