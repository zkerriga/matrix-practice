package matrix

import math.*
import math.aliases.*
import math.syntax.*
import lemmas.given
import matrix.core.echelon.GaussianElimination
import matrix.core.determinant.{DeterminantAlgorithm, LaplaceExpansion}
import matrix.core.LemmaConversions.`Matrix[H, W1 <= W2, A]`

import scala.compiletime.ops.int.*

trait Matrix[Height <: Int, Width <: Int, +A](val height: Height, val width: Width)(using
  val heightEvidence: Evidence[Height > 0],
  val widthEvidence: Evidence[Width > 0],
):
  final def shape: (Height, Width) = height -> width
  final def isSquare: Boolean      = height == width

  def getRow[I <: Int & Singleton](index: I)(using Evidence[I IsIndexFor Height]): Vector[Width, A]
  def getColumn[I <: Int & Singleton](index: I)(using Evidence[I IsIndexFor Width]): Vector[Height, A]

  def topRow: Vector[Width, A] = getRow(0)
  def topTail: Either[Height =:= 1, Matrix[Height - 1, Width, A]]
  def topTail(using Evidence[Height > 1]): Matrix[Height - 1, Width, A]

  def leftColumn: Vector[Height, A] = getColumn(0)
  def leftTail: Either[Width =:= 1, Matrix[Height, Width - 1, A]]
  def leftTail(using Evidence[Width > 1]): Matrix[Height, Width - 1, A] = mapRows(_.tail)

  def addTop[B >: A](top: Vector[Width, B]): Matrix[Height + 1, Width, B]
  def addDown[B >: A](down: Vector[Width, B]): Matrix[Height + 1, Width, B]
  def addLeft[B >: A](left: Vector[Height, B]): Matrix[Height, Width + 1, B]
  def addRight[B >: A](right: Vector[Height, B]): Matrix[Height, Width + 1, B]

  def addTop[Height1 <: Int, B >: A](top: Matrix[Height1, Width, B]): Matrix[Height1 + Height, Width, B]
  def addLeft[Width1 <: Int, B >: A](left: Matrix[Height, Width1, B]): Matrix[Height, Width1 + Width, B]

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

  def transpose: Matrix[Width, Height, A] =
    Matrix.tabulate[Width, Height, A](width, height) { (column, row) =>
      apply(row, column)
    }

  /**
   * alias for the Gaussian Elimination algorithm
   * @see
   *   [[matrix.core.GaussianElimination]] documentaion
   */
  def rowEchelon[A1 >: A: Div: Mul: Sub: Zero: One: Eq]: Matrix[Height, Width, A1] =
    GaussianElimination.on[Height, Width, A1](this)

  def determinant[A1 >: A: Mul: Sub: Add](using
    algorithm: DeterminantAlgorithm[Height] = LaplaceExpansion.on[Height]
  )(using Height =:= Width): A1 = algorithm.det[A1](this)

  def mapRows[Width2 <: Int, B](f: Vector[Width, A] => Vector[Width2, B]): Matrix[Height, Width2, B]
  def map[B](f: A => B): Matrix[Height, Width, B] = mapRows(_.map(f))

  def foldLeft[B](z: B)(op: (B, A) => B): B

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

    def topTail: Either[Height =:= 1, Matrix[Height - 1, Width, A]] =
      table.tail.map(Matrix(_))
    def topTail(using Evidence[Height > 1]): Matrix[Height - 1, Width, A] =
      Matrix(table.tail)

    def leftTail: Either[Width =:= 1, Matrix[Height, Width - 1, A]] =
      import matrix.core.LemmaConversions.`W - 1 > 0 =:= W > 1`
      table.head.tail.map(v => Matrix(table.map(_.tail(using v.sizeEvidence))))

    def addTop[B >: A](vector: Vector[Width, B]): Matrix[Height + 1, Width, B] =
      Matrix(vector +: table)
    def addDown[B >: A](down: Vector[Width, B]): Matrix[Height + 1, Width, B] =
      Matrix(table :+ down)
    def addLeft[B >: A](left: Vector[Height, B]): Matrix[Height, Width + 1, B] =
      Matrix(Vector.map2(left, table)(_ +: _))
    def addRight[B >: A](right: Vector[Height, B]): Matrix[Height, Width + 1, B] =
      Matrix(Vector.map2(table, right)(_ :+ _))

    def addTop[Height1 <: Int, B >: A](top: Matrix[Height1, Width, B]): Matrix[Height1 + Height, Width, B] =
      import top.heightEvidence
      Matrix(Vector.tabulate[Height1, Vector[Width, B]](top.height) { index => top.getRow(index) } ++ table)
    def addLeft[Width1 <: Int, B >: A](left: Matrix[Height, Width1, B]): Matrix[Height, Width1 + Width, B] =
      Matrix(Vector.tabulate[Height, Vector[Width1 + Width, B]](height) { index => left.getRow(index) ++ table(index) })

    def mapRows[Width2 <: Int, B](f: Vector[Width, A] => Vector[Width2, B]): Matrix[Height, Width2, B] =
      Matrix(table.map(f))

    def foldLeft[B](z: B)(op: (B, A) => B): B = table.foldLeft(z) { (acc, vector) => vector.foldLeft(acc)(op) }

    override def toString: String = table.toString
  end Impl

  /* CONSTRUCTORS */

  def apply[Height <: Int, Width <: Int, A](table: Vector[Height, Vector[Width, A]]): Matrix[Height, Width, A] =
    val row = table.head
    import row.sizeEvidence
    import table.sizeEvidence
    Impl(table.size, row.size, table)

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
