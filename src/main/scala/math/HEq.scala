package math

trait HEq[A, B]:
  def equal(a: A, b: B): Boolean

object HEq:
  type Eq[A] = HEq[A, A]

  extension [A](a: A)
    infix def ===[B](b: B)(using he: HEq[A, B]): Boolean = he.equal(a, b)
    infix def =!=[B](b: B)(using HEq[A, B]): Boolean     = !(a === b)

  def fromUniversal[A]: Eq[A] = _ == _

  given Eq[Double] = fromUniversal
  given Eq[Int]    = fromUniversal
