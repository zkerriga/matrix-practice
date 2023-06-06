package matrix

import utils.{Absolute, HMul, Semigroup}

import scala.collection.immutable.Vector as StdVec
import scala.compiletime.ops.int.*

trait Vector[Size <: Int, +A](val size: Size)(using val sizeEvidence: Evidence[Size > 0]):
  def apply[I <: Int & Singleton](index: I)(using Evidence[I IsIndexFor Size]): A
  def head: A

  infix def *[B, C](scalar: B)(using HMul[A, B, C]): Vector[Size, C]         = map(_ *** scalar)
  infix def +[A1 >: A: Semigroup](other: Vector[Size, A1]): Vector[Size, A1] = Vector.map2(this, other)(_ |+| _)
  infix def dot[B, C](other: Vector[Size, B])(using HMul[A, B, C], Semigroup[C]): C =
    Vector.map2(this, other)(_ *** _).reduceLeft(_ |+| _)

  def norm1[A1 >: A: Semigroup: Absolute]: A1 = reduceLeft[A1](_ |+| _).abs
//  def norm: A
//  def normInf: A

  def map[B](f: A => B): Vector[Size, B]
  def reduceLeft[B >: A](op: (B, A) => B): B

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
    def head: A = vec.head

    def map[B](f: A => B): Vector[Size, B] =
      Impl(size, vec.map(f))
    def reduceLeft[B >: A](op: (B, A) => B): B =
      vec.reduceLeft(op)

    override def toString: String = vec.mkString("[", ", ", "]")

  type Make[T <: NonEmptyTuple] = Vector[Tuple.Size[T], Tuple.Union[T]]
  def make(tuple: NonEmptyTuple): Make[tuple.type] =
    Impl(tuple.size, tuple.toList.toVector)(using guaranteed)

  /**
   * [[make]] function returns [[Vector]] with raw types which might be inconvenient. [[of]] method solve the problem by
   * refining the types.
   *
   * But we cannot just return `make(tuple)` expecting `Vector[Size, A]`, scala can't match types even though evidences
   * are provided separately.
   *
   * We need to create a function from `Vector[Tuple.Size[tuple.type], Tuple.Union[tuple.type]]` to `Vector[Size, A]`.
   * Let's see how it works:
   *
   * First, we lift `sizeEvidence` so we get:
   * {{{
   *  val cleanSize: Vector[Tuple.Size[tuple.type], Tuple.Union[tuple.type]] =:= Vector[Size, Tuple.Union[tuple.type]]
   * }}}
   * Given that [[=:=]] is actually a `From => To` function, now we have a function from [[Vector]] with raw size to
   * [[Vector]] with clean size.
   *
   * Second, we lift `unionEvidence` on [[Vector]] with clean size so we get:
   * {{{
   *  val cleanUnion: Vector[Size, Tuple.Union[tuple.type]] <:< Vector[Size, A]
   * }}}
   * Given that [[<:<]] is also a `From => To` function, we can combine both functions to the one which takes [[Vector]]
   * with raw types and returns it with clean types.
   */
  def of[Size <: Int, A](tuple: NonEmptyTuple)(using
    sizeEvidence: Tuple.Size[tuple.type] =:= Size,
    unionEvidence: Tuple.Union[tuple.type] <:< A,
  ): Vector[Size, A] =
    val cleanSize  = sizeEvidence.liftCo[[x] =>> Vector[x & Int, Tuple.Union[tuple.type]]]
    val cleanUnion = unionEvidence.liftCo[[x] =>> Vector[Size, x]]
    cleanSize.andThen(cleanUnion)(make(tuple))

  type OnEvincedIndex[Size <: Int, I <: Int, A] = Evidence[I IsIndexFor Size] ?=> A
  type Tabulate[Size <: Int, A]                 = (index: Int) => OnEvincedIndex[Size, index.type, A]

  def tabulate[Size <: Int, A](size: Size)(f: Tabulate[Size, A])(using Evidence[Size > 0]): Vector[Size, A] =
    Impl(size, StdVec.tabulate(size) { index => f(index)(using guaranteed) })

  def map2[Size <: Int, A, B, C](v1: Vector[Size, A], v2: Vector[Size, B])(
    f: (A, B) => C
  ): Vector[Size, C] =
    import v1.sizeEvidence
    tabulate[Size, C](v1.size) { index => f(v1(index), v2(index)) }
