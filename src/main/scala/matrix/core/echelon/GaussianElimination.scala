package matrix.core.echelon

import math.aliases.*
import math.{Zero, One}
import math.syntax.*
import matrix.{Evidence, Matrix, Vector}
import matrix.core.CatsLikeSyntax.*
import matrix.core.LemmaConversions.given
import matrix.core.MatrixConstructors.*
import matrix.lemmas.given

import scala.annotation.tailrec
import scala.compiletime.ops.int.*

/**
 * My own implementation of Gaussian elimination.
 *
 * The algorithm can be represented by the following steps:
 *   - move to the top of the matrix the row that starts with a non-zero element
 *   - if the whole column starts with zeros, delete the column and run the algorithm on the remaining matrix
 *     - add information about the missing column for back-subtraction to the result
 *   - subtract the top row from all bottom rows to zero out the column on the left, and run the algorithm on the
 *     modified matrix from the bottom right
 *     - subtract from the top vector all the bottom lines from the result
 *     - divide the top vector so that the leading value is one
 *     - combine the divided vector with the zero column and the resulting bottom right matrix
 *
 * @see
 *   [[https://en.wikipedia.org/wiki/Gaussian_elimination]]
 * @note
 *   It is important to note that this algorithm does not claim to be memory efficient, but tries to do as few
 *   operations on numbers as possible. The goal of this part of the project was to practice dynamic programming using
 *   size-parameterized matrices and vectors and additional data structures that carry type proofs. The success criteria
 *   of this algorithm for me are:
 *   - the algorithm is correct and returns a mathematically correct result for any matrices
 *   - the algorithm must be applicable to any types that define mathematical operations by type-classes
 *   - the algorithm does not use the `asInstanceOf` method (the only exception is the mathematical axioms in
 *     [[matrix.lemmas]])
 *   - the algorithm does not perform computational operations on numbers that are logically fixed for the result (for
 *     example, a line that was divided has a `1` at the beginning - this `1` should no longer be used in calculations,
 *     because all upper lines should realize from the algorithm step that everything above this `1` should be zeroed
 *     out using only the coefficients from the upper lines; another example, in the process of subtracting a smaller
 *     line from a larger line, the larger one has values at the beginning that should not be used for subtraction
 *     because underneath them logically there are only zeros, a number minus `0` multiplied by any coefficient remains
 *     the same number - such operations should not be performed)
 *   - operations on matching and verification of entity dimensions must be performed by the compiler (e.g., where a
 *     proof `Size > 1` is expected, the code may use a proof `Size - 1 > 0`, and the conversion must be performed by
 *     the compiler).
 */
object GaussianElimination {
  private case class SubMatrixResult[H <: Int, W <: Int, A](
    subMatrix: Matrix[H, W, A],
    toSubtractAbove: Trapezoid[W - 1, A],
  )

  private def subtractBy[A: Mul: Sub](coefficient: A)(x: A, base: A): A =
    x - base * coefficient

  private def subtractUp[W <: Int, A: Mul: Sub: Zero](
    base: Vector[W, A],
    toSubtract: Trapezoid[W - 1, A],
  ): Node.Processed[W, A] =
    val baseLead: A                                          = base.head
    val maybeBaseTail: Either[W - 1 =:= 0, Vector[W - 1, A]] = base.tail
    toSubtract match
      case Trapezoid.First(down) =>
        down match
          case Trapezoid.ZeroColumn => Node.Skip[W, A](baseLead, Node.Tail[W - 1, A](maybeBaseTail))
          case Node.Tail(maybeDownTail) =>
            val upSubtracted =
              Either.map2(maybeBaseTail, maybeDownTail) { (baseTail, downTail) =>
                Vector.map2(baseTail, downTail)(subtractBy(baseLead))
              }
            Node.Zero(Zero.of[A], Node.Tail(upSubtracted))

      case Trapezoid.Next(down, nextTrap) =>
        val maybeProcessed = maybeBaseTail.map { baseTail => subtractUp(baseTail, nextTrap) }
        down match
          case Trapezoid.ZeroColumn =>
            Node.Skip(baseLead, Node(maybeProcessed))
          case node: Node.Processed[W - 1, A] =>
            Node.Zero(Zero.of[A], Node(maybeProcessed.map(Node.map2(_, node)(subtractBy(baseLead)))))

  private enum SubtractedDown[H <: Int, W <: Int, A]:
    case ToProcess(topTail: Vector[W - 1, A], downRight: Matrix[H - 1, W - 1, A])
    case OnlyTop(topTail: Vector[W - 1, A], hIs1: H =:= 1)
    case OnlyLeft(zeroHeight: H - 1, ev: Evidence[H - 1 > 0], wIs1: W =:= 1)
    case OnlyLead(hIs1: H =:= 1, wIs1: W =:= 1)

  import SubtractedDown.*

  private def subtractDown[H <: Int, W <: Int, A: Div: Mul: Sub: Zero: Eq](
    topVectorLead: A,
    maybeTopVectorTail: Either[W =:= 1, Vector[W - 1, A]],
    maybeTailMatrix: Either[H =:= 1, Matrix[H - 1, W, A]],
  ): SubtractedDown[H, W, A] =
    (maybeTopVectorTail, maybeTailMatrix) match
      case (Right(topVectorTail), Right(tailMatrix)) =>
        val subtractedMatrix = tailMatrix.mapRows { tailMatrixRow =>
          val rowLead = tailMatrixRow.head
          val rowTail = tailMatrixRow.tail(using topVectorTail.sizeEvidence)
          if rowLead === Zero.of[A] then rowTail
          else Vector.map2(rowTail, topVectorTail)(subtractBy(rowLead / topVectorLead))
        }
        ToProcess(topVectorTail, subtractedMatrix)
      case (Right(topVectorTail), Left(hIs1)) => OnlyTop(topVectorTail, hIs1)
      case (Left(wIs1), Right(tailMatrix))    => OnlyLeft(tailMatrix.height, tailMatrix.heightEvidence, wIs1)
      case (Left(wIs1), Left(hIs1))           => OnlyLead(hIs1, wIs1)

  private def onDownRightMatrix[H <: Int, W <: Int, A: Div: Mul: Sub: Zero: One: Eq](
    topLead: A,
    topTail: Vector[W - 1, A],
    downRightMatrixToProcess: Matrix[H - 1, W - 1, A],
  ): SubMatrixResult[H, W, A] =
    val SubMatrixResult(downRightMatrix, toSubtract) = recursive(downRightMatrixToProcess)

    val subtracted: Node.Processed[W - 1, A] = subtractUp(topTail, toSubtract).divideBy(topLead)

    val topVector: Vector[W, A]               = One.of[A] +: subtracted.toVector
    val zeroedDownMatrix: Matrix[H - 1, W, A] = downRightMatrix.mapRows(Zero.of[A] +: _)

    SubMatrixResult(
      zeroedDownMatrix.addTop(topVector),
      Trapezoid.Next(subtracted, toSubtract),
    )

  private def onDownSubtraction[H <: Int, W <: Int, A: Div: Mul: Sub: Zero: One: Eq](
    topLead: A,
    toProcess: SubtractedDown[H, W, A],
  ): SubMatrixResult[H, W, A] =
    toProcess match
      case ToProcess(topTail, downRight) =>
        onDownRightMatrix(topLead, topTail, downRight)
      case OnlyTop(topTail, hIs1) =>
        val dividedTail = topTail.map(_ / topLead)
        SubMatrixResult(
          considering(hIs1) { Matrix(Vector.one[Vector[W, A]](One.of[A] +: dividedTail)) },
          Trapezoid.First(Node.Tail(dividedTail.asRight)),
        )
      case OnlyLeft(height, ev, wIs1) =>
        val column: Vector[H, A] = One.of[A] +: Vector.fill(height)(Zero.of[A])(using ev)
        SubMatrixResult(
          considering(wIs1) { Matrix(Vector.one(column)).transpose },
          Trapezoid.First(Node.Tail(wIs1.asLeft)),
        )
      case OnlyLead(hIs1, wIs1) =>
        SubMatrixResult(
          considering(hIs1, wIs1) { Matrix(Vector.one[Vector[W, A]](Vector.one(One.of[A]))) },
          Trapezoid.First(Node.Tail(wIs1.asLeft)),
        )

  private def onZeroColumn[H <: Int, W <: Int, A: Div: Mul: Sub: Zero: One: Eq](
    height: H,
    maybeRightMatrixToProcess: Either[W =:= 1, Matrix[H, W - 1, A]],
  )(using Evidence[H > 0]): SubMatrixResult[H, W, A] =
    val zeroColumn = Vector.fill(height)(Zero.of[A])
    maybeRightMatrixToProcess match
      case Right(rightMatrixToProcess) =>
        val SubMatrixResult(rightMatrix, toSubtract) = recursive(rightMatrixToProcess)
        SubMatrixResult(
          rightMatrix.addLeft(zeroColumn),
          Trapezoid.Next(Trapezoid.ZeroColumn, toSubtract),
        )
      case Left(wIs1) =>
        SubMatrixResult(
          considering(wIs1) { Matrix(zeroColumn.map(zero => Vector.one(zero))) },
          Trapezoid.First(Trapezoid.ZeroColumn),
        )

  private def moveNonZeroLeadRowToTop[H <: Int, W <: Int, A: Zero: Eq](matrix: Matrix[H, W, A]): Matrix[H, W, A] =
    val topVector = matrix.topRow
    if topVector.head =!= Zero.of[A] then matrix
    else
      matrix.topTail.toOption.fold(matrix) { tailMatrix =>
        @tailrec
        def moving[I <: Int](left: Matrix[H - I, W, A], skipped: Matrix[I, W, A]): Matrix[H, W, A] = {
          val topVector       = left.topRow
          val maybeTailMatrix = left.topTail
          if topVector.head === Zero.of[A] then
            maybeTailMatrix match
              case Right(tailMatrix) => moving(tailMatrix, skipped.addDown(topVector))
              case _                 => matrix
          else
            val rest: Matrix[H - 1, W, A] =
              maybeTailMatrix match
                case Right(tailMatrix) => tailMatrix.addTop(skipped)
                case Left(hIs1)        => considering(hIs1) { skipped }
            rest.addTop(topVector)
        }
        moving[1](tailMatrix, Matrix(Vector.one(topVector)))
      }

  private def recursive[H <: Int, W <: Int, A: Div: Mul: Sub: Zero: One: Eq](
    matrix: Matrix[H, W, A]
  ): SubMatrixResult[H, W, A] = {
    val swapped = moveNonZeroLeadRowToTop(matrix)

    val topVector          = swapped.topRow
    val maybeMatrixTail    = swapped.topTail
    val topVectorLead      = topVector.head
    val maybeTopVectorTail = topVector.tail

    if topVectorLead === Zero.of[A] then
      import matrix.heightEvidence
      onZeroColumn(
        matrix.height,
        maybeTopVectorTail.map { vector =>
          swapped.leftTail(using vector.sizeEvidence)
        },
      )
    else
      onDownSubtraction(
        topVectorLead,
        subtractDown(topVectorLead, maybeTopVectorTail, maybeMatrixTail),
      )
  }

  def on[Height <: Int, Width <: Int, A: Div: Mul: Sub: Zero: One: Eq](
    matrix: Matrix[Height, Width, A]
  ): Matrix[Height, Width, A] = recursive(matrix).subMatrix
}
