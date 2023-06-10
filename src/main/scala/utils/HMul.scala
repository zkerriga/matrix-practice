package utils

/**
 * Heterogeneous polymorphic multiplication
 * @note
 *   thanks to @Odomontois for pointing out Lean typeclasses
 */
trait HMul[-A, -B, +C]:
  def product(x: A, y: B): C

  extension (x: A) infix def ***(y: B): C = product(x, y)

object HMul:
  type Homo[A] = HMul[A, A, A]

  given HMul[String, Int, String] = _ * _
  given HMul.Homo[Int]            = _ * _
  given HMul.Homo[Double]         = _ * _
