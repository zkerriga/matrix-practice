package utils

trait Semigroup[A]:
  def combine(x: A, y: A): A

  extension (x: A) def |+|(y: A): A = combine(x, y)

object Semigroup:
  given Semigroup[String]            = _ + _
  given [A: Numeric]: Semigroup[Int] = _ + _
