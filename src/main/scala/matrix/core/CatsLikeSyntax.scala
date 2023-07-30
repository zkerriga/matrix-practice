package matrix.core

object CatsLikeSyntax:
  extension (o: Either.type)
    def map2[E, A, B, C](aE: Either[E, A], bE: Either[E, B])(f: (A, B) => C): Either[E, C] =
      for
        a <- aE
        b <- bE
      yield f(a, b)

  extension [A](a: A)
    def asRight[L]: Either[L, A] = Right(a)
    def asLeft[R]: Either[A, R]  = Left(a)
