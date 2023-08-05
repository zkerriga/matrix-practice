package matrix

import scala.compiletime.ops.int.*
import scala.compiletime.ops.boolean.*

/**
 * @note
 *   object contains transformations over [[Evidence]] and proofs of [[=:=]] (equality) of expressions. Inequalities and
 *   Equality are mathematical axioms that follow from associativity and equivalence
 */
object lemmas:
  given `A > 1 =:= A - 1 > 0`[A <: Int](using Evidence[A > 1]): Evidence[A - 1 > 0] = guaranteed
  given `A - 1 > 0 =:= A > 1`[A <: Int](using Evidence[A - 1 > 0]): Evidence[A > 1] = guaranteed

  given `A = B =:= A - B = 0`[A <: Int, B <: Int]: =:=[A =:= B, A - B =:= 0]                      = sameGuaranteed
  given `A - B + B =:= A`[A <: Int, B <: Int]: =:=[A - B + B, A]                                  = sameGuaranteed
  given `A - B - C =:= A - (B + C)`[A <: Int, B <: Int, C <: Int]: =:=[A - B - C, A - (B + C)]    = sameGuaranteed
  given `A + (B - A - C) =:= B - C`[A <: Int, B <: Int, C <: Int]: =:=[A + (B - A - C), B - C]    = sameGuaranteed
  given `A - B = C =:= B = A - C`[A <: Int, B <: Int, C <: Int](using A - B =:= C): =:=[B, A - C] = sameGuaranteed
  given `A - B =:= A - 0 - B`[A <: Int, B <: Int]: =:=[A - B, A - 0 - B]                          = sameGuaranteed

  given `A - B - C = D =:= A - C = B + D`[A <: Int, B <: Int, C <: Int, D <: Int]: =:=[A - B - C =:= D, A - C =:= B + D] =
    sameGuaranteed
  given `A - B - C - C =:= A - (B + C) - C`[A <: Int, B <: Int, C <: Int]: =:=[A - B - C - C, A - (B + C) - C] =
    sameGuaranteed
  given `A - B - C =:= A - C if B = 0`[A <: Int, B <: Int, C <: Int](using B =:= 0): =:=[A - B - C, A - C] =
    sameGuaranteed

  given [A <: Boolean, B <: Boolean](using Evidence[A], Evidence[B]): Evidence[A && B] = guaranteed

  given [A <: Int, B <: Int](using Evidence[A > 0], Evidence[B > 0]): Evidence[A + B > 0] = guaranteed
