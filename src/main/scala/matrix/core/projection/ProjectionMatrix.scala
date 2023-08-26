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

    val height = two * (fov / two).tan * near
    val width  = height * ratio

    val x = two * near / width
    val y = two * near / height
    val c = -(far + near) / (far - near)
    val d = -(two * far * near) / (far - near)

    Matrix[4, 4, A] {
      Vector.of(
        Vector.of(x, zero, zero, zero),
        Vector.of(zero, y, zero, zero),
        Vector.of(zero, zero, c, d),
        Vector.of(zero, zero, -one, zero),
      )
    }
