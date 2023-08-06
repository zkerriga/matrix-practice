package matrix.core.determinant

import math.aliases.*
import math.syntax.*
import matrix.{Matrix, Vector}
import matrix.core.MatrixConstructors.*
import matrix.core.LemmaConversions.given
import matrix.lemmas.given

import scala.compiletime.ops.int.*
import scala.annotation.tailrec

object LaplaceExpansion:
  private type DeterminantAcc[A] = A

  @tailrec
  private def f1[S <: Int, I <: Int, A: Mul: Add: Sub](
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
              determinant1x1(Matrix(Vector.of(currentColumn)))
            }
          case Right(rightMatrix) =>
            val right: Matrix[S, S - 1, A]               = considering(iIs0) { rightMatrix }
            val currentColumnTail: Vector[S - 1, A]      = currentColumn.tail(using right.widthEvidence)
            val leftMatrix: Matrix[S - 1, 1, A]          = Matrix(Vector.one(currentColumnTail)).transpose
            val downRightMatrix: Matrix[S - 1, S - 1, A] = right.topTail(using currentColumnTail.sizeEvidence)

            val minorDeterminant: DeterminantAcc[A] = currentColumn.head * determinant(downRightMatrix)

            f1[S, 1, A](
              maybeLeftMatrixAndD = Right(leftMatrix -> minorDeterminant),
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
          case Right(rightMatrix: Matrix[S, S - I - 1, A]) =>
            val rightMatrixTail: Matrix[S - 1, S - I - 1, A] = rightMatrix.topTail(using leftMatrix.heightEvidence)
            val concatendated: Matrix[S - 1, S - 1, A]       = rightMatrixTail.addLeft(leftMatrix)
            val currentColumnTail: Vector[S - 1, A]          = currentColumn.tail(using leftMatrix.heightEvidence)

            val minorDeterminant      = currentColumn.head * determinant(concatendated)
            val updatedDeterminantAcc = signCombinator(determinantAcc, minorDeterminant)

            f1[S, I + 1, A](
              maybeLeftMatrixAndD = Right(leftMatrix.addRight(currentColumnTail) -> updatedDeterminantAcc),
              currentColumn = rightMatrix.leftColumn,
              maybeRightMatrix = rightMatrix.leftTail,
              signCombinator = nextSignCombinator,
              nextSignCombinator = signCombinator,
            )

  def determinant[Size <: Int, A: Mul](matrix: Matrix[Size, Size, A])(using add: Add[A], sub: Sub[A]): A =
    f1[Size, 0, A](
      maybeLeftMatrixAndD = Left(summon[0 =:= 0]),
      currentColumn = matrix.leftColumn,
      maybeRightMatrix = matrix.leftTail,
      signCombinator = add.add,
      nextSignCombinator = sub.subtract,
    )

  def on[Size <: Int]: DeterminantAlgorithm[Size] = new:
    def det[A: Mul: Sub: Add](matrix: Matrix[Size, Size, A]): A = determinant(matrix)
