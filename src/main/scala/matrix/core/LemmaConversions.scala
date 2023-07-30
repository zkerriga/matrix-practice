package matrix.core

import scala.compiletime.ops.int.*
import matrix.lemmas
import matrix.{Evidence, Matrix, Vector}

private[core] object LemmaConversions {
  private def gen[F[_], A, B](using same: A =:= B): Conversion[F[A], F[B]] = fA => same.liftCo[F](fA)

  private def genMatrixH[H1 <: Int, H2 <: Int, W <: Int, A](using
    H1 =:= H2
  ): Conversion[Matrix[H1, W, A], Matrix[H2, W, A]] =
    gen[[h] =>> Matrix[h & Int, W, A], H1, H2]

  private def genMatrixW[W1 <: Int, W2 <: Int, H <: Int, A](using
    W1 =:= W2
  ): Conversion[Matrix[H, W1, A], Matrix[H, W2, A]] =
    gen[[w] =>> Matrix[H, w & Int, A], W1, W2]

  private def genVector[S1 <: Int, S2 <: Int, A](using S1 =:= S2): Conversion[Vector[S1, A], Vector[S2, A]] =
    gen[[s] =>> Vector[s & Int, A], S1, S2]

  private def genEither[E1, E2, A](using E1 =:= E2): Conversion[Either[E1, A], Either[E2, A]] =
    gen[[e] =>> Either[e, A], E1, E2]

  private given [A, B](using same: A =:= B): =:=[B, A] = same.flip

  given `Matrix[H1 => H2, W, A]`[H1 <: Int, H2 <: Int, W <: Int, A](using
    H1 =:= H2
  ): Conversion[Matrix[H1, W, A], Matrix[H2, W, A]] =
    genMatrixH[H1, H2, W, A]

  given `Matrix[H, W1 => W2, A]`[H <: Int, W1 <: Int, W2 <: Int, A](using
    W1 =:= W2
  ): Conversion[Matrix[H, W1, A], Matrix[H, W2, A]] =
    genMatrixW[W1, W2, H, A]

  given `Matrix[H, W1 <= W2, A]`[H <: Int, W1 <: Int, W2 <: Int, A](using
    W2 =:= W1
  ): Conversion[Matrix[H, W1, A], Matrix[H, W2, A]] =
    genMatrixW[W1, W2, H, A]

  given `Vector[S1 => S2, A]`[S1 <: Int, S2 <: Int, A](using S1 =:= S2): Conversion[Vector[S1, A], Vector[S2, A]] =
    genVector[S1, S2, A]

  given `Vector[S1 <= S2, A]`[S1 <: Int, S2 <: Int, A](using S2 =:= S1): Conversion[Vector[S1, A], Vector[S2, A]] =
    genVector[S1, S2, A]

  given `Either[E1 => E2, A]`[E1, E2, A](using E1 =:= E2): Conversion[Either[E1, A], Either[E2, A]] =
    genEither[E1, E2, A]
  given `Either[E1 <= E2, A]`[E1, E2, A](using E2 =:= E1): Conversion[Either[E1, A], Either[E2, A]] =
    genEither[E1, E2, A]

  given `S = 1 =:= S - 1 = 0`[S <: Int]: Conversion[S =:= 1, S - 1 =:= 0] =
    eq => lemmas.`A = B =:= A - B = 0`[S, 1](eq)

  given `W - 1 > 0 =:= W > 1`[W <: Int]: Conversion[Evidence[W - 1 > 0], Evidence[W > 1]] =
    ev => lemmas.`A - 1 > 0 =:= A > 1`(using ev)
}
