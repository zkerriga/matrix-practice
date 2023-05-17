package matrix

import scala.compiletime.ops.int.*

trait SingletonIntPlus[A <: Int & Singleton, B <: Int & Singleton]:
  type Result <: Int & Singleton

object SingletonIntPlus:
  given impl[A <: Int & Singleton, B <: Int & Singleton, R <: Int & Singleton](using
    A + B =:= R
  ): SingletonIntPlus[A, B] = new:
    type Result = R
