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

    val halfHeight = (fov / two).tan * near
    val halfWidth  = halfHeight * ratio

    val left   = -halfWidth
    val right  = halfWidth
    val bottom = -halfHeight
    val top    = halfHeight

    val x = two * near / (right - left)
    val y = two * near / (top - bottom)
    val a = (right + left) / (right - left)
    val b = (top + bottom) / (top - bottom)
    val c = -(far + near) / (far - near)
    val d = -(two * far * near) / (far - near)
    val e = -one

    Matrix[4, 4, A] {
      Vector.of(
        Vector.of(x, zero, a, zero),
        Vector.of(zero, y, b, zero),
        Vector.of(zero, zero, c, d),
        Vector.of(zero, zero, e, zero),
      )
    }
