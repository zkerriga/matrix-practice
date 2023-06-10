package math

@FunctionalInterface
trait Sqrt[A]:
  def squareRoot(a: A): A

  extension (a: A) def sqrt: A = squareRoot(a)

object Sqrt:
  given Sqrt[Double] = scala.math.sqrt
