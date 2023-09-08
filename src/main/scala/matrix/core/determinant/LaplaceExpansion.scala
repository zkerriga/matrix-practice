package matrix.core.determinant

import math.aliases.*
import math.syntax.*
import matrix.core.CatsLikeSyntax.*
import matrix.core.LemmaConversions.given
import matrix.core.MatrixConstructors.*
import matrix.lemmas.given
import matrix.{Matrix, Vector}

import scala.annotation.tailrec
import scala.compiletime.ops.int.*

/**
 * @see
 *   [[https://en.wikipedia.org/wiki/Laplace_expansion]]
 */
object LaplaceExpansion:
  // to make it clear what a determinant accumulator is and what a simple value is
  private type DeterminantAcc[A] = A

  @tailrec
  private def goOverTop[S <: Int, I <: Int, A: Mul: Add: Sub](
    maybeLeftMatrixAndD: Either[I =:= 0, (Matrix[S - 1, I, A], DeterminantAcc[A])],
    currentColumn: Vector[S, A],
    maybeRightMatrix: Either[S - 1 =:= I, Matrix[S, S - I - 1, A]],
    signCombinator: (DeterminantAcc[A], A) => DeterminantAcc[A],
    nextSignCombinator: (DeterminantAcc[A], A) => DeterminantAcc[A],
  ): DeterminantAcc[A] =
    maybeLeftMatrixAndD match
      case Left(iIs0) =>
        maybeRightMatrix match
          case Left(sMinus1IsI) =>
            considering(iIs0, sMinus1IsI) {
              determinant1x1(Matrix.oneRow(currentColumn))
            }

          case Right(rightMatrix) =>
            import right.widthEvidence
            val right: Matrix[S, S - 1, A]               = considering(iIs0) { rightMatrix }
            val downRightMatrix: Matrix[S - 1, S - 1, A] = right.topTail
            val leftMatrix                               = Matrix.oneColumn(currentColumn.tail)

            val minorDeterminant = currentColumn.head * determinant(downRightMatrix)

            goOverTop[S, 1, A](
              maybeLeftMatrixAndD = (leftMatrix, minorDeterminant).asRight,
              currentColumn = right.leftColumn,
              maybeRightMatrix = right.leftTail,
              signCombinator = nextSignCombinator,
              nextSignCombinator = signCombinator,
            )

      case Right((leftMatrix, determinantAcc)) =>
        maybeRightMatrix match
          case Left(sMinus1IsI) =>
            val left: Matrix[S - 1, S - 1, A] = considering(sMinus1IsI) { leftMatrix }
            val minorDeterminant              = currentColumn.head * determinant(left)
            signCombinator(determinantAcc, minorDeterminant)

          case Right(rightMatrix) =>
            import leftMatrix.heightEvidence
            val concatendated: Matrix[S - 1, S - 1, A] = rightMatrix.topTail.addLeft(leftMatrix)

            val minorDeterminant      = currentColumn.head * determinant(concatendated)
            val updatedDeterminantAcc = signCombinator(determinantAcc, minorDeterminant)

            goOverTop[S, I + 1, A](
              maybeLeftMatrixAndD = (leftMatrix.addRight(currentColumn.tail), updatedDeterminantAcc).asRight,
              currentColumn = rightMatrix.leftColumn,
              maybeRightMatrix = rightMatrix.leftTail,
              signCombinator = nextSignCombinator,
              nextSignCombinator = signCombinator,
            )

  def determinant[Size <: Int, A: Mul](matrix: Matrix[Size, Size, A])(using add: Add[A], sub: Sub[A]): A =
    goOverTop[Size, 0, A](
      maybeLeftMatrixAndD = summon[0 =:= 0].asLeft,
      currentColumn = matrix.leftColumn,
      maybeRightMatrix = matrix.leftTail,
      signCombinator = add.add,
      nextSignCombinator = sub.subtract,
    )

  def on[Size <: Int]: DeterminantAlgorithm[Size] = new:
    def det[A: Mul: Sub: Add](matrix: Matrix[Size, Size, A]): A = determinant(matrix)
