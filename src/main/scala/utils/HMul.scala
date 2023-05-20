package utils

/**
 * Heterogeneous polymorphic multiplication
 * @note
 *   thanks to @Odomontois for pointing out Lean typeclasses
 */
trait HMul[-A, -B, +C]:
  def product(x: A, y: B): C

  extension (x: A) def ***(y: B): C = product(x, y)

object HMul:
  given HMul[String, Int, String] = _ * _
  given HMul[Int, Int, Int]       = _ * _
