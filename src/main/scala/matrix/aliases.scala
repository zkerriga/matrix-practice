package matrix

import java.util.concurrent.locks.Condition
import scala.compiletime.ops.boolean.*
import scala.compiletime.ops.int.*

type Evidence[Condition <: Boolean] = Condition =:= true

infix type IsIndexFor[I <: Int, Size <: Int] = I >= 0 && Size > I

private[matrix] def guaranteed[Condition <: Boolean]: Evidence[Condition] =
  <:<.refl.asInstanceOf[Evidence[Condition]]

private[matrix] def sameGuaranteed[A, B]: =:=[A, B] = <:<.refl.asInstanceOf
