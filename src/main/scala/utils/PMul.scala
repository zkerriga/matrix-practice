package utils

/**
  * Polymorphic multiplication. (named after corresponding Lean typeclass)
  */
trait PMul[-A, -B, C]:
  def product(x: A, y: B): C

  extension (x: A) def ***(y: B): C = product(x, y)

object PMul:
  given PMul[Int, Int, Int]       = _ * _
  given PMul[String, Int, String] = _ * _
