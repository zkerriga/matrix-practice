package utils

/**
 * Heterogeneous polymorphic multiplication
 * @note
 *   thanks to @Odomontois for pointing out Lean typeclasses
 */
trait HMul[-A, -B, +C]:
  def product(x: A, y: B): C

object HMul:
  given HMul[String, Int, String]         = _ * _
  given [A: Numeric]: HMul[Int, Int, Int] = _ * _
