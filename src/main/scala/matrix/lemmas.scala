package matrix

import scala.compiletime.ops.int.*
import scala.compiletime.ops.boolean.*

object lemmas:
  /**
   * for any integer A and B if A > B than B < A
   */
  given [A <: Int, B <: Int](using Evidence[A > B]): Evidence[B < A] = guaranteed

  /**
   * for any boolean A and B if A is true and B is true than A && B is also true
   */
  given [A <: Boolean, B <: Boolean](using Evidence[A], Evidence[B]): Evidence[A && B] = guaranteed

  given [A <: Int](using Evidence[A > 1]): Evidence[(A - 1) > 0] = guaranteed
