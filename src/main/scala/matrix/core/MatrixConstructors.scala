package matrix.core

import matrix.core.LemmaConversions.given
import matrix.lemmas.given
import matrix.{Matrix, Vector}

import scala.compiletime.ops.int.*

private[core] object MatrixConstructors:
  def considering[Size, A](size: Size)(f: Size ?=> A): A = {
    given Size = size
    f
  }
  def considering[H, W, A](h: H, w: W)(f: (H, W) ?=> A): A = {
    given H = h
    given W = w
    f
  }

  def desplitTop[H <: Int, W <: Int, A](
    top: Vector[W, A],
    tail: Either[H =:= 1, Matrix[H - 1, W, A]],
  ): Matrix[H, W, A] =
    tail match
      case Right(tailMatrix) => tailMatrix.addTop(top)
      case Left(hIs1)        => considering(hIs1) { Matrix(Vector.one(top)) }

  def desplit[Size <: Int, A](lead: A, tail: Either[Size =:= 1, Vector[Size - 1, A]]): Vector[Size, A] =
    tail match
      case Right(tailVector) => lead +: tailVector
      case Left(sizeIs1)     => considering(sizeIs1) { Vector.one(lead) }
