package matrix

import utils.{HMul, Semigroup}

import scala.collection.immutable.Vector as StdVec
import scala.compiletime.ops.int.*

trait Vector[Size <: Int, +A](val size: Size)(using Evidence[Size > 0]):
  def apply[I <: Int & Singleton](index: I)(using Evidence[I IsIndexFor Size]): A

  def *[B, C](scalar: B)(using HMul[A, B, C]): Vector[Size, C]
  def +[A1 >: A: Semigroup](other: Vector[Size, A1]): Vector[Size, A1]

  override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match
    case other: Vector[Size @unchecked, A @unchecked] =>
      (size == other.size) && (0 until size).forall { index =>
        given Evidence[index.type IsIndexFor Size] = guaranteed
        apply(index) == other.apply(index)
      }
    case _ => false
  )

object Vector:
  private class Impl[Size <: Int, +A](size: Size, vec: StdVec[A])(using Evidence[Size > 0])
      extends Vector[Size, A](size):
    def apply[I <: Int & Singleton](index: I)(using Evidence[I IsIndexFor Size]): A =
      vec(index)

    def *[B, C](scalar: B)(using HMul[A, B, C]): Vector[Size, C] =
      Impl(size, vec.map(_ *** scalar))
    def +[A1 >: A: Semigroup](other: Vector[Size, A1]): Vector[Size, A1] =
      tabulate[Size, A1](size) { index => vec(index) |+| other(index) }

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

  type OnEvincedIndex[Size <: Int, I <: Int, A] = Evidence[I IsIndexFor Size] ?=> A
  type Tabulate[Size <: Int, A] = (index: Int) => OnEvincedIndex[Size, index.type, A]

  def tabulate[Size <: Int, A](size: Size)(f: Tabulate[Size, A])(using
    Evidence[Size > 0]
  ): Vector[Size, A] = Impl(size, StdVec.tabulate(size) { index => f(index)(using guaranteed) })
