package math

trait HDiv[-A, -B, +C]:
  def divide(a: A, b: B): C

object HDiv:
  type Div[A] = HDiv[A, A, A]

  extension [A](a: A) infix def /[B, C](b: B)(using hd: HDiv[A, B, C]): C = hd.divide(a, b)

  given Div[Double] = _ / _
