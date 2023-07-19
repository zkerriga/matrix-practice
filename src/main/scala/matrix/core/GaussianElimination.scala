package matrix.core

import math.Zero
import matrix.lemmas.given

import scala.collection.immutable.Vector as StdVector
import matrix.{Evidence, Matrix, Vector}

import scala.annotation.tailrec
import scala.compiletime.ops.int.{+, -, >}

private[matrix] object GaussianElimination:
  type NonEmptyList[+A] = List[A]

  case class SubMatrixResult[H <: Int, W <: Int, +A](
    subMatrix: Matrix[H, W, A],
    // todo: vectors or lists?
    toSubtractAbove: StdVector[NonEmptyList[Option[Vector[_, A]]]],
  )

  extension [H <: Int, W <: Int, A, F[_ <: Int, _ <: Int, _]](fa: F[H, W, A])
    def asH[H2 <: Int](using H =:= H2): F[H2, W, A] =
      summon[H =:= H2].liftCo[[h] =>> F[h & Int, W, A]](fa)
    def asW[W2 <: Int](using W =:= W2): F[H, W2, A] =
      summon[W =:= W2].liftCo[[w] =>> F[H, w & Int, A]](fa)

  def moveNonZeroLeadRowToTop[H <: Int, W <: Int, A: Zero](matrix: Matrix[H, W, A]): Matrix[H, W, A] = {
    val topVector = matrix.topRow
    if topVector.head != Zero.of[A] then matrix
    else
      matrix.topTail.fold(matrix) { tailMatrix =>
        @tailrec
        def moving[I <: Int](left: Matrix[H - I, W, A], skipped: Matrix[I, W, A]): Matrix[H, W, A] = {
          val topVector       = left.topRow
          val maybeTailMatrix = left.topTail
          if topVector.head == Zero.of[A] then
            maybeTailMatrix match
              case Some(tailMatrix) => moving(tailMatrix.asH[H - (I + 1)], skipped.addDown(topVector))
              case None             => matrix
          else
            (maybeTailMatrix match
              case Some(tailMatrix) => tailMatrix.addTop(skipped).asH[H - 1]
              case None             => skipped.asInstanceOf[Matrix[H - 1, W, A]]
            ).addTop(topVector).asH[H]
        }
        moving[1](tailMatrix, Matrix { Vector.of(topVector) })
      }
  }

  def onZeroColumn[H <: Int, W <: Int, A: Zero](
    height: H,
    maybeRightMatrixToProcess: Option[Matrix[H, W, A]],
  )(using Evidence[H > 0]): SubMatrixResult[H, W + 1, A] =
    val zero                     = Zero.of[A]
    val zeroColumn: Vector[H, A] = Vector.fill(height)(zero)
    maybeRightMatrixToProcess match
      case Some(rightMatrixToProcess) =>
        val SubMatrixResult(rightMatrix, toBeSubtractedAbove) = recursive(rightMatrixToProcess)
        SubMatrixResult(rightMatrix.addLeft(zeroColumn), toBeSubtractedAbove)
      case None =>
        SubMatrixResult(Matrix(zeroColumn.map(Vector.of[A](_).asInstanceOf[Vector[W + 1, A]])), StdVector.empty)

  def desplitTop[H <: Int, W <: Int, A](top: Vector[W, A], tail: Option[Matrix[H - 1, W, A]]): Matrix[H, W, A] =
    tail match
      case Some(tailMatrix) => tailMatrix.addTop(top).asH[H]
      case None             => Matrix(Vector.of[Vector[W, A]](top).asInstanceOf[Vector[H, Vector[W, A]]])

  def dropZeroColumn[H <: Int, W <: Int, A](
    topVectorTail: Vector[W - 1, A],
    maybeTailMatrix: Option[Matrix[H - 1, W, A]],
  ): Matrix[H, W - 1, A] =
    import topVectorTail.sizeEvidence
    desplitTop(
      topVectorTail,
      maybeTailMatrix.map(_.leftTail),
    )

  def recursive[H <: Int, W <: Int, A: Zero](matrix: Matrix[H, W, A]): SubMatrixResult[H, W, A] = {
    val swapped = moveNonZeroLeadRowToTop(matrix)

    val topVector          = swapped.topRow
    val maybeMatrixTail    = swapped.topTail
    val topVectorLead      = topVector.head
    val maybeTopVectorTail = topVector.tail

    if topVectorLead == Zero.of[A] then
      import matrix.heightEvidence
      onZeroColumn(
        matrix.height,
        maybeTopVectorTail.map(dropZeroColumn(_, maybeMatrixTail)),
      ).asW[W]
    else ???
  }

@main def test = {
  val matrix: Matrix[4, 2, Int] = Matrix {
    Vector.of(
      Vector.of(0, 1),
      Vector.of(0, 2),
      Vector.of(1, 2),
      Vector.of(0, 3),
    )
  }

  val matrix2: Matrix[4, 2, Int] = Matrix {
    Vector.of(
      Vector.of(0, 0),
      Vector.of(0, 0),
      Vector.of(0, 0),
      Vector.of(0, 0),
    )
  }
  println(GaussianElimination.recursive(matrix2))
}
