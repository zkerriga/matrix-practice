package matrix

import java.util.concurrent.locks.Condition

type Evidence[Condition <: Boolean] = Condition =:= true

private[matrix] def guaranteed[Condition <: Boolean]: Evidence[Condition] =
  <:<.refl.asInstanceOf[Evidence[Condition]]
