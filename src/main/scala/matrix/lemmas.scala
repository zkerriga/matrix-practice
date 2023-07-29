package matrix

import scala.compiletime.ops.int.*
import scala.compiletime.ops.boolean.*

object lemmas:
  given `A > 1 =:= A - 1 > 0`[A <: Int]: =:=[A > 1, A - 1 > 0] = sameGuaranteed

  given [A <: Boolean, B <: Boolean](using Evidence[A], A =:= B): Evidence[B] =
    summon[A =:= B].liftCo[[x] =>> Evidence[x & Boolean]](summon[Evidence[A]])

  given [A <: Boolean, B <: Boolean](using Evidence[A], Evidence[B]): Evidence[A && B] = guaranteed

  given [A <: Int, B <: Int](using Evidence[A > 0], Evidence[B > 0]): Evidence[A + B > 0] = guaranteed
