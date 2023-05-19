package matrix

import scala.Tuple.{Size, Union}
import scala.compiletime.ops.int.*
import scala.compiletime.ops.boolean.*
import scala.collection.immutable.Vector as StdVec

trait Vector[Size <: Int, +A](val size: Size)(using Evidence[Size > 0]):
  def apply[I <: Int & Singleton](index: I)(using Evidence[I >= 0 && I < Size]): A

object Vector:
  private class Impl[Size <: Int, +A](size: Size, vec: StdVec[A])(using Evidence[Size > 0])
      extends Vector[Size, A](size):
    def apply[I <: Int & Singleton](index: I)(using Evidence[I >= 0 && I < Size]): A =
      vec(index)

    override def toString: String = vec.mkString("[", ", ", "]")

  type Make[T <: NonEmptyTuple] = Vector[Tuple.Size[T], Tuple.Union[T]]
  def make(tuple: NonEmptyTuple): Make[tuple.type] =
    Impl(tuple.size, tuple.toList.toVector)(using guaranteed)

  /**
   * [[make]] function returns [[Vector]] with raw types which might be inconvenient. [[of]] method
   * solve the problem by refining the types.
   *
   * But we cannot just return `make(tuple)` expecting `Vector[Size, A]`, scala can't match types
   * even though evidences are provided separately. We need to create a function from
   * `Vector[Tuple.Size[tuple.type], Tuple.Union[tuple.type]]` to `Vector[Size, A]`. Let's see how
   * it works:
   *
   * First, we lift `sizeEvidence` so we get:
   * {{{
   *  val cleanSize: Vector[Tuple.Size[tuple.type], Tuple.Union[tuple.type]] =:= Vector[Size, Tuple.Union[tuple.type]]
   * }}}
   * Given that [[=:=]] is actually a `From => To` function, now we have a function from [[Vector]]
   * with raw size to [[Vector]] with clean size.
   *
   * Second, we lift `unionEvidence` on [[Vector]] with clean size so we get:
   * {{{
   *  val cleanUnion: Vector[Size, Tuple.Union[tuple.type]] <:< Vector[Size, A]
   * }}}
   * Given that [[<:<]] is also a `From => To` function, we can combine both functions to the one
   * which takes [[Vector]] with raw types and returns it with clean types.
   */
  def of[Size <: Int, A](tuple: NonEmptyTuple)(using
    sizeEvidence: Tuple.Size[tuple.type] =:= Size,
    unionEvidence: Tuple.Union[tuple.type] <:< A,
  ): Vector[Size, A] =
    val cleanSize  = sizeEvidence.liftCo[[x] =>> Vector[x & Int, Tuple.Union[tuple.type]]]
    val cleanUnion = unionEvidence.liftCo[[x] =>> Vector[Size, x]]
    cleanSize.andThen(cleanUnion)(make(tuple))
