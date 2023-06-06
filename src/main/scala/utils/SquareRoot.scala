package utils

@FunctionalInterface
trait SquareRoot[A]:
  def squareRoot(x: A): A

  extension (x: A) def sqrt: A = squareRoot(x)

object SquareRoot:
  given SquareRoot[Double] = scala.math.sqrt
