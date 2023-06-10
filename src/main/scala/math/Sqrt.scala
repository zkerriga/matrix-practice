package math

trait Sqrt[A]:
  def squareRoot(a: A): A

  extension (a: A) def sqrt: A = squareRoot(a)
