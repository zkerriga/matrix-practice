package matrix

import math.*
import math.aliases.*
import math.syntax.*

import scala.compiletime.ops.int.*

trait Matrix[Height <: Int, Weight <: Int, +A](val height: Height, val weight: Weight)(using
  val heightEvidence: Evidence[Height > 0],
  val weightEvidence: Evidence[Weight > 0],
):
  final def shape: (Height, Weight) = height -> weight
  final def isSquare: Boolean       = height == weight

  def getRow[I <: Int & Singleton](index: I)(using Evidence[I IsIndexFor Height]): Vector[Weight, A]
  def getColumn[I <: Int & Singleton](index: I)(using Evidence[I IsIndexFor Weight]): Vector[Height, A]

  def apply[Row <: Int & Singleton, Column <: Int & Singleton](row: Row, column: Column)(using
    Evidence[Row IsIndexFor Height],
    Evidence[Column IsIndexFor Weight],
  ): A = getRow[Row](row).apply[Column](column)

  def *[B, C](scalar: B)(using HMul[A, B, C]): Matrix[Height, Weight, C] = map(_ * scalar)
  def +[B, C](other: Matrix[Height, Weight, B])(using HAdd[A, B, C]): Matrix[Height, Weight, C] =
    Matrix.map2(this, other)(_ + _)
  def -[B, C](other: Matrix[Height, Weight, B])(using HSub[A, B, C]): Matrix[Height, Weight, C] =
    Matrix.map2(this, other)(_ - _)

  def linearMap[B, C](vector: Vector[Weight, B])(using HMul[A, B, C], Add[C]): Vector[Height, C] =
    Vector.tabulate[Height, C](height) { row => getRow(row) dot vector }

  /** computes a matrix that is the multiplication of two matrices */
  infix def x[Weight2 <: Int, B, C](
    other: Matrix[Weight, Weight2, B]
  )(using HMul[A, B, C], Add[C]): Matrix[Height, Weight2, C] =
    import other.weightEvidence
    Matrix.tabulate[Height, Weight2, C](height, other.weight) { (row, column) =>
      this.getRow(row) dot other.getColumn(column)
    }

  def map[B](f: A => B): Matrix[Height, Weight, B]

  override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match
    case other: Matrix[Height @unchecked, Weight @unchecked, A @unchecked] =>
      (shape == other.shape) && (0 until height).forall { row =>
        given Evidence[row.type IsIndexFor Height] = guaranteed
        (0 until weight).forall { column =>
          given Evidence[x.type IsIndexFor Weight] = guaranteed
          apply(row, column) == other.apply(row, column)
        }
      }
    case _ => false
  )

object Matrix:
  private class Impl[Height <: Int, Weight <: Int, +A](
    height: Height,
    weight: Weight,
    table: Vector[Height, Vector[Weight, A]],
  )(using Evidence[Height > 0], Evidence[Weight > 0])
      extends Matrix[Height, Weight, A](height, weight):
    def getRow[I <: Int & Singleton](index: I)(using Evidence[I IsIndexFor Height]): Vector[Weight, A] =
      table.apply[I](index)

    def getColumn[I <: Int & Singleton](index: I)(using Evidence[I IsIndexFor Weight]): Vector[Height, A] =
      Vector.tabulate[Height, A](height) { row => getRow(row).apply[I](index) }

    def map[B](f: A => B): Matrix[Height, Weight, B] =
      Impl(height, weight, table.map(_.map(f)))

    override def toString: String = table.toString
  end Impl

  /* CONSTRUCTORS */

  def apply[Height <: Int, Weight <: Int, A](table: Vector[Height, Vector[Weight, A]])(using
    ValueOf[Height],
    ValueOf[Weight],
  ): Matrix[Height, Weight, A] =
    val row = table.head
    import row.sizeEvidence
    import table.sizeEvidence
    Impl(valueOf, valueOf, table)

  type OnEvincedIndexes[Height <: Int, Weight <: Int, Row <: Int, Column <: Int, A] =
    (Evidence[Row IsIndexFor Height], Evidence[Column IsIndexFor Weight]) ?=> A
  type Tabulate[Height <: Int, Weight <: Int, A] =
    (row: Int, column: Int) => OnEvincedIndexes[Height, Weight, row.type, column.type, A]

  /**
   * creates a new [[Matrix]] of the passed `height` and `weight` using `f` to calculate each value by its index pair
   * @param f
   *   function that allows you to make calculations using an index pair having also the proof that the index pair is a
   *   valid index pair for a matrix of a certain shape
   * @note
   *   uses [[Vector.tabulate]]
   */
  def tabulate[Height <: Int, Weight <: Int, A](height: Height, weight: Weight)(
    f: Tabulate[Height, Weight, A]
  )(using Evidence[Height > 0], Evidence[Weight > 0]): Matrix[Height, Weight, A] =
    Impl(
      height,
      weight,
      Vector.tabulate[Height, Vector[Weight, A]](height) { row =>
        Vector.tabulate[Weight, A](weight) { column =>
          f(row, column)(using guaranteed)
        }
      },
    )

  def diagonal[Size <: Int, A: Zero](diagonal: A)(using ValueOf[Size], Evidence[Size > 0]): Matrix[Size, Size, A] =
    tabulate[Size, Size, A](valueOf, valueOf) { (y, x) => if y == x then diagonal else Zero.of[A] }

  def identity[Size <: Int, A: One: Zero](using ValueOf[Size], Evidence[Size > 0]): Matrix[Size, Size, A] =
    diagonal(One.of[A])

  def map2[Height <: Int, Weight <: Int, A, B, C](
    m1: Matrix[Height, Weight, A],
    m2: Matrix[Height, Weight, B],
  )(f: (A, B) => C): Matrix[Height, Weight, C] =
    import m1.{heightEvidence, weightEvidence}
    tabulate[Height, Weight, C](m1.height, m1.weight) { (row, column) =>
      f(m1(row, column), m2(row, column))
    }

  /* ADDITIONAL MATH OPERATIONS */

  /** generates [[Interpolation]] for any [[Matrix]] which type can be interpolated */
  given [Height <: Int, Weight <: Int, A, Time](using
    Interpolation[A, Time]
  ): Interpolation[Matrix[Height, Weight, A], Time] =
    (from, to, time) => map2(from, to)((value1, value2) => (value1, value2).interpolateBy(time))
