package utils

trait Semigroup[A]:
  def combine(x: A, y: A): A

object Semigroup:
  given Semigroup[String] = _ + _
  given Semigroup[Int]    = _ + _
