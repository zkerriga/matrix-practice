package utils

@FunctionalInterface
trait Absolute[A]:
  def absolute(x: A): A

  extension (x: A) def abs: A = absolute(x)

object Absolute:
  given Absolute[String] = identity
  given Absolute[Double] = x => if x <= 0.0d then 0.0d - x else x
