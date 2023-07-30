package matrix

import scala.compiletime.ops.int.*
import scala.compiletime.ops.boolean.*

object lemmas:
  given `A > 1 =:= A - 1 > 0`[A <: Int](using Evidence[A > 1]): Evidence[A - 1 > 0] = guaranteed
  given `A - 1 > 0 =:= A > 1`[A <: Int](using Evidence[A - 1 > 0]): Evidence[A > 1] = guaranteed

  given `A = B =:= A - B = 0`[A <: Int, B <: Int]: =:=[A =:= B, A - B =:= 0] = sameGuaranteed
  given `A - B + B =:= A`[A <: Int, B <: Int]: =:=[A - B + B, A]             = sameGuaranteed

  given [A <: Boolean, B <: Boolean](using Evidence[A], Evidence[B]): Evidence[A && B] = guaranteed

  given [A <: Int, B <: Int](using Evidence[A > 0], Evidence[B > 0]): Evidence[A + B > 0] = guaranteed
