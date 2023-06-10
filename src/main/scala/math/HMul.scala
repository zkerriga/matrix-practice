package math

trait HMul[-A, -B, +C]:
  def product(a: A, b: B): C

object HMul:
  type Mul[A] = HMul[A, A, A]

  extension [A](a: A) infix def *[B, C](b: B)(using hm: HMul[A, B, C]): C = hm.product(a, b)

  given Mul[Double] = _ * _
