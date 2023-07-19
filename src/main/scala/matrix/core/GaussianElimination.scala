package matrix.core

import math.Zero
import matrix.lemmas.given

import scala.collection.immutable.Vector as StdVector
import matrix.{Matrix, Vector}

import scala.annotation.tailrec
import scala.compiletime.ops.int.{+, -}

private[matrix] object GaussianElimination:
  type NonEmptyList[+A] = List[A]

  case class SubMatrixResult[H <: Int, W <: Int, +A](
    subMatrix: Matrix[H, W, A],
    // todo: vectors or lists?
    toSubtractAbove: StdVector[NonEmptyList[Option[Vector[_, A]]]],
  )

  extension [H <: Int, W <: Int, A](matrix: Matrix[H, W, A])
    def asH[H2 <: Int](using H =:= H2): Matrix[H2, W, A] =
      summon[H =:= H2].liftCo[[h] =>> Matrix[h & Int, W, A]](matrix)

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

  def recursive[H <: Int, W <: Int, A: Zero](matrix: Matrix[H, W, A]): SubMatrixResult[H, W, A] =
    SubMatrixResult(moveNonZeroLeadRowToTop(matrix), StdVector.empty)

@main def test = {
  val matrix: Matrix[4, 2, Int] = Matrix {
    Vector.of(
      Vector.of(0, 1),
      Vector.of(0, 2),
      Vector.of(1, 2),
      Vector.of(0, 3),
    )
  }
  println(GaussianElimination.recursive(matrix))
}
