package math

trait HSub[-A, -B, +C]:
  def subtract(a: A, b: B): C

object HSub:
  type Sub[A] = HSub[A, A, A]

  extension [A](a: A) infix def -[B, C](b: B)(using hs: HSub[A, B, C]): C = hs.subtract(a, b)
