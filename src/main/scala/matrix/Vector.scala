package matrix

import math.*
import math.aliases.*
import math.syntax.*
import lemmas.given

import scala.collection.immutable.Vector as StdVec
import scala.compiletime.ops.int.{>, -, +}
import scala.math.Ordering.Implicits.*

trait Vector[Size <: Int, +A](val size: Size)(using val sizeEvidence: Evidence[Size > 0]):
  def apply[I <: Int & Singleton](index: I)(using Evidence[I IsIndexFor Size]): A
  def tail: Either[Size =:= 1, Vector[Size - 1, A]]
  def tail(using Evidence[Size > 1]): Vector[Size - 1, A]
  def init: Either[Size =:= 1, Vector[Size - 1, A]]

  def +:[B >: A](a: B): Vector[Size + 1, B]
  def :+[B >: A](a: B): Vector[Size + 1, B]
  def ++[S <: Int, B >: A](other: Vector[S, B]): Vector[Size + S, B]

  def head: A = apply(0)
  def last: A = apply[(Size - 1) & Singleton]((size - 1).asInstanceOf[(Size - 1) & Singleton])

  infix def *[B, C](scalar: B)(using HMul[A, B, C]): Vector[Size, C]              = map(_ * scalar)
  infix def +[B, C](other: Vector[Size, B])(using HAdd[A, B, C]): Vector[Size, C] = Vector.map2(this, other)(_ + _)
  infix def -[B, C](other: Vector[Size, B])(using HSub[A, B, C]): Vector[Size, C] = Vector.map2(this, other)(_ - _)

  /** computes the dot product of two [[Vector]]s of the same dimension */
  infix def dot[B, C](other: Vector[Size, B])(using HMul[A, B, C], Add[C]): C =
    Vector.map2(this, other)(_ * _).sum

  /** alias for the cross product operation, see [[Vector.crossProduct]] */
  infix def x[B, C](other: Vector[3, B])(using Size =:= 3, HMul[A, B, C], Sub[C]): Vector[3, C] =
    Vector.crossProduct(
      // using provided evidence we clarify the type of `this` from `Vector[Size, A]` to `Vector[3, A]`
      summon[Size =:= 3].liftCo[[x] =>> Vector[x & Int, A]](this),
      other,
    )

  /** 1-norm (also called the Taxicab norm or Manhattan norm) */
  def norm1[A1 >: A: Add: Abs]: A1 = sum.abs

  /** 2-norm (also called the Euclidean norm) */
  def norm[A1 >: A: Mul: Add: Sqrt]: A1 = map(x => x * x).sum.sqrt

  /** ∞-norm (also called the supremum norm) */
  def normInf[A1 >: A: Abs: Ordering]: A1 = reduceLeft { (a, b) => a.abs max b.abs }

  def map[B](f: A => B): Vector[Size, B]
  def reduceLeft[B >: A](op: (B, A) => B): B
  def foldLeft[B](z: B)(op: (B, A) => B): B

  def sum[A1 >: A: Add]: A1 = reduceLeft(_ + _)

  override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match
    case other: Vector[Size @unchecked, A @unchecked] =>
      (size == other.size) && (0 until size).forall { index =>
        given Evidence[index.type IsIndexFor Size] = guaranteed
        apply(index) == other.apply(index)
      }
    case _ => false
  )

object Vector:
  extension [Size <: Int](size: Size)
    private infix def |-[S <: Int](s: S): Size - S = (size - s).asInstanceOf
    private infix def |+[S <: Int](s: S): Size + S = (size + s).asInstanceOf

  private class Impl[Size <: Int, +A](size: Size, vec: StdVec[A])(using Evidence[Size > 0])
      extends Vector[Size, A](size):
    def apply[I <: Int & Singleton](index: I)(using Evidence[I IsIndexFor Size]): A =
      vec(index)

    def tail: Either[Size =:= 1, Vector[Size - 1, A]] =
      Either.cond(size > 1, Impl(size |- 1, vec.tail)(using guaranteed), sameGuaranteed)

    def tail(using Evidence[Size > 1]): Vector[Size - 1, A] =
      Impl(size |- 1, vec.tail)

    def init: Either[Size =:= 1, Vector[Size - 1, A]] =
      Either.cond(size > 1, Impl(size |- 1, vec.init)(using guaranteed), sameGuaranteed)

    def +:[B >: A](a: B): Vector[Size + 1, B] =
      Impl(size |+ 1, a +: vec)
    def :+[B >: A](a: B): Vector[Size + 1, B] =
      Impl(size |+ 1, vec :+ a)

    def ++[S <: Int, B >: A](other: Vector[S, B]): Vector[Size + S, B] =
      import other.sizeEvidence
      Impl(size |+ other.size, vec ++ StdVec.tabulate(other.size)(index => other(index)(using guaranteed)))

    def map[B](f: A => B): Vector[Size, B] =
      Impl(size, vec.map(f))
    def reduceLeft[B >: A](op: (B, A) => B): B = vec.reduceLeft(op)
    def foldLeft[B](z: B)(op: (B, A) => B): B  = vec.foldLeft(z)(op)

    override def toString: String = vec.mkString("[", ", ", "]")
  end Impl

  /* CONSTRUCTORS */

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

  def of[A](value: A): Vector[1, A] = make(value *: EmptyTuple)

  type OnEvincedIndex[Size <: Int, I <: Int, A] = Evidence[I IsIndexFor Size] ?=> A
  type Tabulate[Size <: Int, A]                 = (index: Int) => OnEvincedIndex[Size, index.type, A]

  /**
   * creates a new [[Vector]] of the passed `size` using `f` to calculate each value by its index
   * @param f
   *   function that allows you to make calculations using an index having also the proof that the index is a valid
   *   index for a vector of a certain size
   */
  def tabulate[Size <: Int, A](size: Size)(f: Tabulate[Size, A])(using Evidence[Size > 0]): Vector[Size, A] =
    Impl(size, StdVec.tabulate(size) { index => f(index)(using guaranteed) })

  def fill[Size <: Int, A](size: Size)(elem: => A)(using Evidence[Size > 0]): Vector[Size, A] =
    Impl(size, StdVec.fill(size)(elem))

  /**
   * similar to [[tabulate]], but instead of creating a [[Vector]] it immediately reduces all the values
   * @param op
   *   function for reducing the elements obtained from `f`
   */
  def tabulateReduce[Size <: Int, A](size: Size, op: (A, A) => A)(f: Tabulate[Size, A])(using Evidence[Size > 0]): A =
    (1 until size).foldLeft[A](f(0)(using guaranteed)) { (acc, index) =>
      op(acc, f(index)(using guaranteed))
    }

  def map2[Size <: Int, A, B, C](v1: Vector[Size, A], v2: Vector[Size, B])(
    f: (A, B) => C
  ): Vector[Size, C] =
    import v1.sizeEvidence
    tabulate[Size, C](v1.size) { index => f(v1(index), v2(index)) }

  /* ADDITIONAL MATH OPERATIONS */

  /** generates [[Interpolation]] for any [[Vector]] which type can be interpolated */
  given [Size <: Int, A, Time](using Interpolation[A, Time]): Interpolation[Vector[Size, A], Time] =
    (from, to, time) => map2(from, to)((value1, value2) => (value1, value2).interpolateBy(time))

  /** computes a linear combination of the [[Vector]]s provided, using the corresponding scalar coefficients */
  def linearCombination[N <: Int, Size <: Int, A, B, C](
    vectors: Vector[N, Vector[Size, A]],
    coefficients: Vector[N, B],
  )(using HMul[A, B, C], Add[C]): Vector[Size, C] =
    map2(vectors, coefficients)(_ * _).reduceLeft(_ + _)

  /** computes the cosine of the angle between two given [[Vector]]s of the same dimension */
  def angleCos[Size <: Int, A: Sqrt: Mul: Add, B: Sqrt: Mul: Add, C: Div: Add](
    vA: Vector[Size, A],
    vB: Vector[Size, B],
  )(using HMul[A, B, C]): C =
    (vA dot vB) / (vA.norm * vB.norm)

  /** computes the cross product of two 3-dimensional [[Vector]]s */
  def crossProduct[A, B, C: Sub](vA: Vector[3, A], vB: Vector[3, B])(using HMul[A, B, C]): Vector[3, C] =
    Vector.of(
      vA(1) * vB(2) - vA(2) * vB(1),
      vA(2) * vB(0) - vA(0) * vB(2),
      vA(0) * vB(1) - vA(1) * vB(0),
    )
