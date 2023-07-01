package matrix

import math.*
import math.aliases.*
import math.syntax.*

import scala.compiletime.ops.int.*

trait Matrix[Height <: Int, Width <: Int, +A](val height: Height, val width: Width)(using
  val heightEvidence: Evidence[Height > 0],
  val widthEvidence: Evidence[Width > 0],
):
  final def shape: (Height, Width) = height -> width
  final def isSquare: Boolean      = height == width

  def getRow[I <: Int & Singleton](index: I)(using Evidence[I IsIndexFor Height]): Vector[Width, A]
  def getColumn[I <: Int & Singleton](index: I)(using Evidence[I IsIndexFor Width]): Vector[Height, A]

  def apply[Row <: Int & Singleton, Column <: Int & Singleton](row: Row, column: Column)(using
    Evidence[Row IsIndexFor Height],
    Evidence[Column IsIndexFor Width],
  ): A = getRow[Row](row).apply[Column](column)

  def *[B, C](scalar: B)(using HMul[A, B, C]): Matrix[Height, Width, C] = map(_ * scalar)
  def +[B, C](other: Matrix[Height, Width, B])(using HAdd[A, B, C]): Matrix[Height, Width, C] =
    Matrix.map2(this, other)(_ + _)
  def -[B, C](other: Matrix[Height, Width, B])(using HSub[A, B, C]): Matrix[Height, Width, C] =
    Matrix.map2(this, other)(_ - _)

  def linearMap[B, C](vector: Vector[Width, B])(using HMul[A, B, C], Add[C]): Vector[Height, C] =
    Vector.tabulate[Height, C](height) { row => getRow(row) dot vector }

  /** computes a matrix that is the multiplication of two matrices */
  infix def x[Width2 <: Int, B, C](
    other: Matrix[Width, Width2, B]
  )(using HMul[A, B, C], Add[C]): Matrix[Height, Width2, C] =
    import other.widthEvidence
    Matrix.tabulate[Height, Width2, C](height, other.width) { (row, column) =>
      this.getRow(row) dot other.getColumn(column)
    }

  def trace[A1 >: A: Add](using squareEvidence: Height =:= Width): A1 =
    Vector.tabulateReduce[Height, A1](height, _ + _) { index =>
      /* using the evidence that `Height` is `Width` we prove that
       * provided by tabulation `Height index evidence` is the same as `Width index evidence`
       */
      given Evidence[index.type IsIndexFor Width] =
        squareEvidence
          .liftCo[[size] =>> Evidence[index.type IsIndexFor size & Int]]
          .apply(summon[Evidence[index.type IsIndexFor Height]])
      apply(index, index)
    }

  def map[B](f: A => B): Matrix[Height, Width, B]

  override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match
    case other: Matrix[Height @unchecked, Width @unchecked, A @unchecked] =>
      (shape == other.shape) && (0 until height).forall { row =>
        given Evidence[row.type IsIndexFor Height] = guaranteed
        (0 until width).forall { column =>
          given Evidence[column.type IsIndexFor Width] = guaranteed
          apply(row, column) == other.apply(row, column)
        }
      }
    case _ => false
  )

object Matrix:
  private class Impl[Height <: Int, Width <: Int, +A](
    height: Height,
    width: Width,
    table: Vector[Height, Vector[Width, A]],
  )(using Evidence[Height > 0], Evidence[Width > 0])
      extends Matrix[Height, Width, A](height, width):
    def getRow[I <: Int & Singleton](index: I)(using Evidence[I IsIndexFor Height]): Vector[Width, A] =
      table.apply[I](index)

    def getColumn[I <: Int & Singleton](index: I)(using Evidence[I IsIndexFor Width]): Vector[Height, A] =
      Vector.tabulate[Height, A](height) { row => getRow(row).apply[I](index) }

    def map[B](f: A => B): Matrix[Height, Width, B] =
      Impl(height, width, table.map(_.map(f)))

    override def toString: String = table.toString
  end Impl

  /* CONSTRUCTORS */

  def apply[Height <: Int, Width <: Int, A](table: Vector[Height, Vector[Width, A]])(using
    ValueOf[Height],
    ValueOf[Width],
  ): Matrix[Height, Width, A] =
    val row = table.head
    import row.sizeEvidence
    import table.sizeEvidence
    Impl(valueOf, valueOf, table)

  type OnEvincedIndexes[Height <: Int, Width <: Int, Row <: Int, Column <: Int, A] =
    (Evidence[Row IsIndexFor Height], Evidence[Column IsIndexFor Width]) ?=> A
  type Tabulate[Height <: Int, Width <: Int, A] =
    (row: Int, column: Int) => OnEvincedIndexes[Height, Width, row.type, column.type, A]

  /**
   * creates a new [[Matrix]] of the passed `height` and `width` using `f` to calculate each value by its index pair
   * @param f
   *   function that allows you to make calculations using an index pair having also the proof that the index pair is a
   *   valid index pair for a matrix of a certain shape
   * @note
   *   uses [[Vector.tabulate]]
   */
  def tabulate[Height <: Int, Width <: Int, A](height: Height, width: Width)(
    f: Tabulate[Height, Width, A]
  )(using Evidence[Height > 0], Evidence[Width > 0]): Matrix[Height, Width, A] =
    Impl(
      height,
      width,
      Vector.tabulate[Height, Vector[Width, A]](height) { row =>
        Vector.tabulate[Width, A](width) { column =>
          f(row, column)(using guaranteed)
        }
      },
    )

  def diagonal[Size <: Int, A: Zero](diagonal: A)(using ValueOf[Size], Evidence[Size > 0]): Matrix[Size, Size, A] =
    tabulate[Size, Size, A](valueOf, valueOf) { (y, x) => if y == x then diagonal else Zero.of[A] }

  def identity[Size <: Int, A: One: Zero](using ValueOf[Size], Evidence[Size > 0]): Matrix[Size, Size, A] =
    diagonal(One.of[A])

  def map2[Height <: Int, Width <: Int, A, B, C](
    m1: Matrix[Height, Width, A],
    m2: Matrix[Height, Width, B],
  )(f: (A, B) => C): Matrix[Height, Width, C] =
    import m1.{heightEvidence, widthEvidence}
    tabulate[Height, Width, C](m1.height, m1.width) { (row, column) =>
      f(m1(row, column), m2(row, column))
    }

  /* ADDITIONAL MATH OPERATIONS */

  /** generates [[Interpolation]] for any [[Matrix]] which type can be interpolated */
  given [Height <: Int, Width <: Int, A, Time](using
    Interpolation[A, Time]
  ): Interpolation[Matrix[Height, Width, A], Time] =
    (from, to, time) => map2(from, to)((value1, value2) => (value1, value2).interpolateBy(time))
