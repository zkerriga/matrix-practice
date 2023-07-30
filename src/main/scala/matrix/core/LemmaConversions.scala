package matrix.core

import scala.compiletime.ops.int.*
import matrix.lemmas
import matrix.{Evidence, Matrix, Vector}

private[core] object LemmaConversions {
  private def gen[F[_ <: Int], A <: Int, B <: Int](using same: A =:= B): Conversion[F[A], F[B]] =
    fA => same.liftCo[[x] =>> F[x & Int]](fA)

  private def genVector[S1 <: Int, S2 <: Int, A](using S1 =:= S2): Conversion[Vector[S1, A], Vector[S2, A]] =
    gen[[s] =>> Vector[s & Int, A], S1, S2]

  private given [A, B](using same: A =:= B): =:=[B, A] = same.flip

  given c1[S <: Int, A]: Conversion[Either[S - 1 =:= 0, A], Either[S =:= 1, A]] = _.asInstanceOf
  given c2[S <: Int, A]: Conversion[Either[S =:= 1, A], Either[S - 1 =:= 0, A]] = _.asInstanceOf

  given c3[H <: Int, W <: Int, A]: Conversion[Matrix[H - 1 + 1, W, A], Matrix[H, W, A]]        = _.asInstanceOf
  given c10[H <: Int, W <: Int, A]: Conversion[Matrix[H, W - 1 + 1, A], Matrix[H, W, A]]       = _.asInstanceOf
  given c8[H <: Int, W <: Int, A](using W =:= 1): Conversion[Matrix[H, 1, A], Matrix[H, W, A]] = _.asInstanceOf
  given c9[H <: Int, W <: Int, A](using H =:= 1, W =:= 1): Conversion[Matrix[1, 1, A], Matrix[H, W, A]] =
    _.asInstanceOf
  given c11[H <: Int, I <: Int, W <: Int, A]: Conversion[Matrix[H - I - 1, W, A], Matrix[H - (I + 1), W, A]] =
    _.asInstanceOf
  given c12[H <: Int, I <: Int, W <: Int, A]: Conversion[Matrix[I + (H - I - 1), W, A], Matrix[H - 1, W, A]] =
    _.asInstanceOf
  given c13[H <: Int, I <: Int, W <: Int, A](using H - I =:= 1): Conversion[Matrix[I, W, A], Matrix[H - 1, W, A]] =
    _.asInstanceOf

  given `Vector[S1 => S2, A]`[S1 <: Int, S2 <: Int, A](using S1 =:= S2): Conversion[Vector[S1, A], Vector[S2, A]] =
    genVector[S1, S2, A]

  given `Vector[S1 <= S2, A]`[S1 <: Int, S2 <: Int, A](using S2 =:= S1): Conversion[Vector[S1, A], Vector[S2, A]] =
    genVector[S1, S2, A]

  given `S = 1 =:= S - 1 = 0`[S <: Int]: Conversion[S =:= 1, S - 1 =:= 0] =
    eq => lemmas.`A = B =:= A - B = 0`[S, 1](eq)

  given `W - 1 > 0 =:= W > 1`[W <: Int]: Conversion[Evidence[W - 1 > 0], Evidence[W > 1]] =
    ev => lemmas.`A - 1 > 0 =:= A > 1`(using ev)
}
