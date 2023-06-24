package math

@FunctionalInterface
trait Abs[A]:
  def absolute(a: A): A

  extension (x: A) def abs: A = absolute(x)

object Abs:
  given Abs[Double] = scala.math.abs
  given Abs[Int]    = scala.math.abs
