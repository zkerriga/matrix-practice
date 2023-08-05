package matrix.core

import scala.compiletime.ops.int.*
import matrix.lemmas
import matrix.{Evidence, Matrix, Vector}

/**
 * @note
 *   object contains several [[Conversion]]s for convenient work with [[Matrix]] and [[Vector]] operations that are
 *   derived from proofs from [[matrix.lemmas]]
 *
 * @example
 *   imagine you're doing the operation of taking the first element from a [[Vector]] and attaching another:
 *   {{{
 *     val tail = vector.tail
 *     val result = element +: tail
 *   }}}
 *   the `result` type will be [[Vector]] with `Size - 1 + 1`, which is the same as `Size`. Scala cannot derive type
 *   equality automatically, but with these [[Conversion]]s it is sufficient to simply specify the final type for the
 *   operation to convert the size automatically:
 *   {{{
 *     val tail = vector.tail
 *     val result: Vector[Size, A] = element +: tail
 *   }}}
 */
private[matrix] object LemmaConversions:
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

  given `Matrix[S1 => S2, S1 => S2, A]`[S1 <: Int, S2 <: Int, A](using
    S1 =:= S2
  ): Conversion[Matrix[S1, S1, A], Matrix[S2, S2, A]] =
    m1 => `Matrix[H, W1 => W2, A]`[S2, S1, S2, A](`Matrix[H1 => H2, W, A]`[S1, S2, S1, A](m1))

  given `Vector[S1 => S2, A]`[S1 <: Int, S2 <: Int, A](using S1 =:= S2): Conversion[Vector[S1, A], Vector[S2, A]] =
    genVector[S1, S2, A]

  given `Vector[S1 <= S2, A]`[S1 <: Int, S2 <: Int, A](using S2 =:= S1): Conversion[Vector[S1, A], Vector[S2, A]] =
    genVector[S1, S2, A]

  given `Either[E1 => E2, A]`[E1, E2, A](using E1 =:= E2): Conversion[Either[E1, A], Either[E2, A]] =
    genEither[E1, E2, A]
  given `Either[E1 <= E2, A]`[E1, E2, A](using E2 =:= E1): Conversion[Either[E1, A], Either[E2, A]] =
    genEither[E1, E2, A]

  given `Either[E1 => E2, Matrix[H, W1 => W2, A]]`[E1, E2, H <: Int, W1 <: Int, W2 <: Int, A](using
    W1 =:= W2,
    E1 =:= E2,
  ): Conversion[Either[E1, Matrix[H, W1, A]], Either[E2, Matrix[H, W2, A]]] =
    e1 => `Either[E1 => E2, A]`(e1).map(`Matrix[H, W1 => W2, A]`)

  given `S = 1 =:= S - 1 = 0`[S <: Int]: Conversion[S =:= 1, S - 1 =:= 0] =
    eq => lemmas.`A = B =:= A - B = 0`[S, 1](eq)

  given `W - 1 > 0 =:= W > 1`[W <: Int]: Conversion[Evidence[W - 1 > 0], Evidence[W > 1]] =
    ev => lemmas.`A - 1 > 0 =:= A > 1`(using ev)
