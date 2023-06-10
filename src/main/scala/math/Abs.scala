package math

trait Abs[A]:
  def absolute(a: A): A

  extension (x: A) def abs: A = absolute(x)
