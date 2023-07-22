package matrix

import scala.compiletime.ops.int.*
import scala.compiletime.ops.boolean.*
import org.apache.commons.math3.analysis.function.Sin

object lemmas:
  /**
   * for any integer A and B if A > B than B < A
   */
  given [A <: Int, B <: Int](using Evidence[A > B]): Evidence[B < A] = guaranteed

  /**
   * for any boolean A and B if A is true and B is true than A && B is also true
   */
  given [A <: Boolean, B <: Boolean](using Evidence[A], Evidence[B]): Evidence[A && B] = guaranteed

  given l1[A <: Int, B <: Int](using Evidence[A > B]): Evidence[(A - B) > 0] = guaranteed

  given l2[A <: Int, B <: Int](using Evidence[(A - B) > 0]): Evidence[A > B] = guaranteed

  // todo: document
  given [A <: Int](using Evidence[A > 0]): Evidence[(A - 1) >= 0] = guaranteed

  // todo: document
  given [A <: Int]: Evidence[(A - 1) < A] = guaranteed

  // todo: remove
  given [A <: Int, B <: Int](using Evidence[A IsIndexFor B]): Evidence[A & Singleton IsIndexFor B] = guaranteed

  /**
   * for any two integers > 0 the sum of them will be > 0
   */
  given [A <: Int, B <: Int](using Evidence[A > 0], Evidence[B > 0]): Evidence[(A + B) > 0] = guaranteed

  /**
   * for any integer B minus and plus together do nothing with the A
   */
  given [A <: Int, B <: Int]: =:=[A - B + B, A] = sameGuaranteed

  given [A <: Int, B <: Int, C <: Int](using (A - B) =:= C): =:=[B, A - C] = sameGuaranteed

  /**
   * Associative Property of Addition
   */
  given [A <: Int, B <: Int, C <: Int]: =:=[A - B - C, A - (B + C)] = sameGuaranteed

  /**
   * for any integers A minus A will do nothing for B - C
   */
  given [A <: Int, B <: Int, C <: Int]: =:=[A + (B - A - C), B - C] = sameGuaranteed

  given [A <: Int, B <: Int]: =:=[A - (A - B), B] = sameGuaranteed
