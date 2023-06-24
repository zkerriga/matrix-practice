package math

trait Zero[+A](val value: A)

object Zero:
  def apply[A](zero: A): Zero[A] = new Zero(zero) {}

  def of[A](using zero: Zero[A]): A = zero.value

  given Zero[Double] = Zero(0.0)
