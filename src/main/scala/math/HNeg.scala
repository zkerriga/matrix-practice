package math

trait HNeg[-A, +B]:
  def negate(a: A): B

object HNeg:
  type Neg[A] = HNeg[A, A]

  extension [A, B](a: A) def unary_-(using n: HNeg[A, B]): B = n.negate(a)

  import math.Zero
  import math.aliases.Sub
  import math.syntax.*
  given [A](using Zero[A], Sub[A]): Neg[A] = a => Zero.of[A] - a
