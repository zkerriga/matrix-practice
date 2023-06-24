package matrix

import math.*
import math.aliases.*
import math.syntax.*

import scala.compiletime.ops.int.*

trait Matrix[Weight <: Int, Height <: Int, +A](val weight: Weight, val height: Height)(using
  val weightEvidence: Evidence[Weight > 0],
  val heightEvidence: Evidence[Height > 0],
):
  final def shape: (Weight, Height) = weight -> height
  final def isSquare: Boolean       = weight == height

  def getRow[Row <: Int & Singleton](row: Row)(using Evidence[Row IsIndexFor Height]): Vector[Weight, A]
  def getColumn[Column <: Int & Singleton](column: Column)(using Evidence[Column IsIndexFor Weight]): Vector[Height, A]

  def apply[Row <: Int & Singleton, Column <: Int & Singleton](row: Row, column: Column)(using
    Evidence[Row IsIndexFor Height],
    Evidence[Column IsIndexFor Weight],
  ): A = getRow[Row](row).apply[Column](column)

  def *[B, C](scalar: B)(using HMul[A, B, C]): Matrix[Weight, Height, C] = map(_ * scalar)
  def +[B, C](other: Matrix[Weight, Height, B])(using HAdd[A, B, C]): Matrix[Weight, Height, C] =
    Matrix.map2(this, other)(_ + _)
  def -[B, C](other: Matrix[Weight, Height, B])(using HSub[A, B, C]): Matrix[Weight, Height, C] =
    Matrix.map2(this, other)(_ - _)

  def linearMap[B, C](vector: Vector[Weight, B])(using HMul[A, B, C], Add[C]): Vector[Height, C] =
    Vector.tabulate[Height, C](height) { row => getRow(row) dot vector }

  /** computes a matrix that is the multiplication of two matrices */
  infix def x[Weight2 <: Int, B, C](
    other: Matrix[Weight2, Weight, B]
  )(using HMul[A, B, C], Add[C]): Matrix[Weight2, Height, C] =
    import other.weightEvidence
    Matrix.tabulate[Weight2, Height, C](other.weight, height) { (row, column) =>
      this.getRow(row) dot other.getColumn(column)
    }

  def map[B](f: A => B): Matrix[Weight, Height, B]

  override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match
    case other: Matrix[Weight @unchecked, Height @unchecked, A @unchecked] =>
      (shape == other.shape) && (0 until height).forall { y =>
        given Evidence[y.type IsIndexFor Height] = guaranteed
        (0 until weight).forall { x =>
          given Evidence[x.type IsIndexFor Weight] = guaranteed
          apply(y, x) == other.apply(y, x)
        }
      }
    case _ => false
  )

object Matrix:
  private class Impl[Weight <: Int, Height <: Int, +A](
    weight: Weight,
    height: Height,
    table: Vector[Height, Vector[Weight, A]],
  )(using Evidence[Weight > 0], Evidence[Height > 0])
      extends Matrix[Weight, Height, A](weight, height):
    def getRow[Row <: Int & Singleton](row: Row)(using Evidence[Row IsIndexFor Height]): Vector[Weight, A] =
      table.apply[Row](row)

    def getColumn[Column <: Int & Singleton](
      column: Column
    )(using Evidence[Column IsIndexFor Weight]): Vector[Height, A] =
      Vector.tabulate[Height, A](height) { row =>
        getRow(row).apply[Column](column)
      }

    def map[B](f: A => B): Matrix[Weight, Height, B] =
      Impl(weight, height, table.map(_.map(f)))

    override def toString: String = table.toString
  end Impl

  /* CONSTRUCTORS */

  def apply[Weight <: Int, Height <: Int, A](table: Vector[Height, Vector[Weight, A]])(using
    ValueOf[Weight],
    ValueOf[Height],
  ): Matrix[Weight, Height, A] =
    val row = table.head
    import row.sizeEvidence
    import table.sizeEvidence
    Impl(valueOf, valueOf, table)

  type OnEvincedIndexes[Weight <: Int, Height <: Int, Y <: Int, X <: Int, A] =
    (Evidence[Y IsIndexFor Height], Evidence[X IsIndexFor Weight]) ?=> A
  type Tabulate[Weight <: Int, Height <: Int, A] =
    (y: Int, x: Int) => OnEvincedIndexes[Weight, Height, y.type, x.type, A]

  /**
   * creates a new [[Matrix]] of the passed `weight` and `height` using `f` to calculate each value by its index pair
   * @param f
   *   function that allows you to make calculations using an index pair having also the proof that the index pair is a
   *   valid index pair for a matrix of a certain shape
   * @note
   *   uses [[Vector.tabulate]]
   */
  def tabulate[Weight <: Int, Height <: Int, A](weight: Weight, height: Height)(
    f: Tabulate[Weight, Height, A]
  )(using Evidence[Weight > 0], Evidence[Height > 0]): Matrix[Weight, Height, A] =
    Impl(
      weight,
      height,
      Vector.tabulate[Height, Vector[Weight, A]](height) { y =>
        Vector.tabulate[Weight, A](weight) { x =>
          f(y, x)(using guaranteed)
        }
      },
    )

  def diagonal[Size <: Int, A: Zero](diagonal: A)(using ValueOf[Size], Evidence[Size > 0]): Matrix[Size, Size, A] =
    tabulate[Size, Size, A](valueOf, valueOf) { (y, x) => if y == x then diagonal else Zero.of[A] }

  def identity[Size <: Int, A: One: Zero](using ValueOf[Size], Evidence[Size > 0]): Matrix[Size, Size, A] =
    diagonal(One.of[A])

  def map2[Weight <: Int, Height <: Int, A, B, C](
    m1: Matrix[Weight, Height, A],
    m2: Matrix[Weight, Height, B],
  )(f: (A, B) => C): Matrix[Weight, Height, C] =
    import m1.{heightEvidence, weightEvidence}
    tabulate[Weight, Height, C](m1.weight, m1.height) { (y, x) =>
      f(m1(y, x), m2(y, x))
    }

  /* ADDITIONAL MATH OPERATIONS */

  /** generates [[Interpolation]] for any [[Matrix]] which type can be interpolated */
  given [Weight <: Int, Height <: Int, A, Time](using
    Interpolation[A, Time]
  ): Interpolation[Matrix[Weight, Height, A], Time] =
    (from, to, time) => map2(from, to)((value1, value2) => (value1, value2).interpolateBy(time))
