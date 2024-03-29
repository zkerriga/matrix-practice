package matrix

import math.*
import math.aliases.*
import math.syntax.*
import matrix.core.LemmaConversions.{`Matrix[H, W1 <= W2, A]`, `W - 1 > 0 =:= W > 1`}
import matrix.core.determinant.{DeterminantAlgorithm, LaplaceExpansion}
import matrix.core.echelon.GaussianElimination
import matrix.core.projection.ProjectionMatrix
import matrix.core.inverse.{DeterminantGaussianElimination, InverseAlgorithm}
import matrix.lemmas.given

import scala.compiletime.ops.int.*

trait Matrix[Height <: Int, Width <: Int, +A](val height: Height, val width: Width)(using
  val heightEvidence: Evidence[Height > 0],
  val widthEvidence: Evidence[Width > 0],
):
  final def shape: (Height, Width) = height -> width

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

  infix def *[B, C](scalar: B)(using HMul[A, B, C]): Matrix[Height, Width, C] =
    map(_ * scalar)
  infix def +[B, C](other: Matrix[Height, Width, B])(using HAdd[A, B, C]): Matrix[Height, Width, C] =
    Matrix.map2(this, other)(_ + _)
  infix def -[B, C](other: Matrix[Height, Width, B])(using HSub[A, B, C]): Matrix[Height, Width, C] =
    Matrix.map2(this, other)(_ - _)

  def linearMap[B, C](vector: Vector[Width, B])(using HMul[A, B, C], Add[C]): Vector[Height, C] =
    Vector.tabulate[Height, C](height): row =>
      getRow(row) dot vector

  /** computes a matrix that is the multiplication of two matrices */
  infix def x[Width2 <: Int, B, C](
    other: Matrix[Width, Width2, B]
  )(using HMul[A, B, C], Add[C]): Matrix[Height, Width2, C] =
    import other.widthEvidence
    Matrix.tabulate[Height, Width2, C](height, other.width): (row, column) =>
      this.getRow(row) dot other.getColumn(column)

  def trace[A1 >: A: Add](using Height =:= Width): A1 =
    val square: Matrix[Height, Height, A] = this
    Vector.tabulateReduce[Height, A1](height, _ + _): index =>
      square(index, index)

  def transpose: Matrix[Width, Height, A] =
    Matrix.tabulate[Width, Height, A](width, height): (column, row) =>
      apply(row, column)

  /**
   * alias for the Gaussian Elimination algorithm
   * @see
   *   [[matrix.core.GaussianElimination]] documentaion
   */
  def rowEchelon[A1 >: A: Div: Mul: Sub: Zero: One: Eq]: Matrix[Height, Width, A1] =
    GaussianElimination.on[Height, Width, A1](this) match
      case (matrix, _) => matrix

  def determinant[A1 >: A: Mul: Sub: Add](using
    algorithm: DeterminantAlgorithm[Height] = LaplaceExpansion.on[Height]
  )(using Height =:= Width): A1 = algorithm.det[A1](this)

  def inverse[A1 >: A: Div: Mul: Sub: Add: Zero: One: Eq](using
    algorithm: InverseAlgorithm[Height] = DeterminantGaussianElimination.on()
  )(using Height =:= Width): Option[Matrix[Height, Height, A1]] = algorithm.inv[A1](this)

  def rank[A1 >: A: Div: Mul: Sub: Zero: One: Eq]: Int =
    GaussianElimination.on[Height, Width, A1](this) match
      case (_, rank) => rank.get

  def mapRows[Width2 <: Int, B](f: Vector[Width, A] => Vector[Width2, B]): Matrix[Height, Width2, B]
  def map[B](f: A => B): Matrix[Height, Width, B] = mapRows(_.map(f))

  def foldLeft[B](z: B)(op: (B, A) => B): B

  override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match
    case other: Matrix[Height @unchecked, Width @unchecked, A @unchecked] =>
      (shape == other.shape) && (0 until height).forall: row =>
        given Evidence[row.type IsIndexFor Height] = guaranteed
        (0 until width).forall: column =>
          given Evidence[column.type IsIndexFor Width] = guaranteed
          apply(row, column) == other.apply(row, column)
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
      Vector.tabulate[Height, A](height): row =>
        getRow(row).apply[I](index)

    def topTail: Either[Height =:= 1, Matrix[Height - 1, Width, A]] =
      table.tail.map(Matrix(_))
    def topTail(using Evidence[Height > 1]): Matrix[Height - 1, Width, A] =
      Matrix(table.tail)

    def leftTail: Either[Width =:= 1, Matrix[Height, Width - 1, A]] =
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
      Matrix {
        Vector.tabulate[Height, Vector[Width1 + Width, B]](height): index =>
          left.getRow(index) ++ table(index)
      }

    def mapRows[Width2 <: Int, B](f: Vector[Width, A] => Vector[Width2, B]): Matrix[Height, Width2, B] =
      Matrix(table.map(f))

    def foldLeft[B](z: B)(op: (B, A) => B): B =
      table.foldLeft(z): (acc, vector) =>
        vector.foldLeft(acc)(op)

    override def toString: String = table.toString
  end Impl

  /* CONSTRUCTORS */

  def apply[Height <: Int, Width <: Int, A](table: Vector[Height, Vector[Width, A]]): Matrix[Height, Width, A] =
    val row = table.head
    Impl(table.size, row.size, table)(using table.sizeEvidence, row.sizeEvidence)

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
      Vector.tabulate[Height, Vector[Width, A]](height): row =>
        Vector.tabulate[Width, A](width): column =>
          f(row, column)(using guaranteed),
    )

  def diagonal[Size <: Int, A: Zero](size: Size, diagonalA: A)(using Evidence[Size > 0]): Matrix[Size, Size, A] =
    tabulate[Size, Size, A](size, size) { (y, x) => if y == x then diagonalA else Zero.of[A] }

  def diagonal[Size <: Int, A: Zero](diagonalA: A)(using ValueOf[Size], Evidence[Size > 0]): Matrix[Size, Size, A] =
    diagonal[Size, A](valueOf, diagonalA)

  def identity[Size <: Int, A: One: Zero](size: Size)(using Evidence[Size > 0]): Matrix[Size, Size, A] =
    diagonal(size, One.of[A])

  def identity[Size <: Int, A: One: Zero](using ValueOf[Size], Evidence[Size > 0]): Matrix[Size, Size, A] =
    identity[Size, A](valueOf)

  def oneColumn[Height <: Int, A](column: Vector[Height, A]): Matrix[Height, 1, A] =
    Matrix(column.map(Vector.one))

  def oneRow[Width <: Int, A](row: Vector[Width, A]): Matrix[1, Width, A] =
    Matrix(Vector.one(row))

  def projection[A: Tan: Div: Mul: Sub: Add: One: Zero](fov: A, ratio: A, near: A, far: A): Matrix[4, 4, A] =
    ProjectionMatrix.from(fov, ratio, near, far)

  def map2[Height <: Int, Width <: Int, A, B, C](
    matrixA: Matrix[Height, Width, A],
    matrixB: Matrix[Height, Width, B],
  )(f: (A, B) => C): Matrix[Height, Width, C] =
    import matrixA.{heightEvidence, widthEvidence}
    tabulate[Height, Width, C](matrixA.height, matrixA.width): (row, column) =>
      f(matrixA(row, column), matrixB(row, column))

  /* ADDITIONAL MATH OPERATIONS */

  /** generates [[Interpolation]] for any [[Matrix]] which type can be interpolated */
  given [Height <: Int, Width <: Int, A, Time](using
    Interpolation[A, Time]
  ): Interpolation[Matrix[Height, Width, A], Time] = (from, to, time) =>
    map2(from, to): (value1, value2) =>
      (value1, value2).interpolateBy(time)
