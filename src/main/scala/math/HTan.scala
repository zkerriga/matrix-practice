package math

trait HTan[-A, +B]:
  def tangent(angle: A): B

object HTan:
  type Tan[A] = HTan[A, A]

  extension [A, B](a: A) def tan(using t: HTan[A, B]): B = t.tangent(a)

  given degree: Tan[Double] = angle => scala.math.tan(angle.toRadians)
