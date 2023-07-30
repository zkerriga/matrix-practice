package matrix.core

import matrix.{Evidence, Matrix, Vector}

import scala.compiletime.ops.int.*

private[core] object MatrixConversions:
  given c1[S <: Int, A]: Conversion[Either[S - 1 =:= 0, A], Either[S =:= 1, A]] = _.asInstanceOf
  given c2[S <: Int, A]: Conversion[Either[S =:= 1, A], Either[S - 1 =:= 0, A]] = _.asInstanceOf

  given c3[H <: Int, W <: Int, A]: Conversion[Matrix[H - 1 + 1, W, A], Matrix[H, W, A]]        = _.asInstanceOf
  given c10[H <: Int, W <: Int, A]: Conversion[Matrix[H, W - 1 + 1, A], Matrix[H, W, A]]       = _.asInstanceOf
  given c8[H <: Int, W <: Int, A](using W =:= 1): Conversion[Matrix[H, 1, A], Matrix[H, W, A]] = _.asInstanceOf
  given c9[H <: Int, W <: Int, A](using H =:= 1, W =:= 1): Conversion[Matrix[1, 1, A], Matrix[H, W, A]] =
    _.asInstanceOf
  given c11[H <: Int, I <: Int, W <: Int, A]: Conversion[Matrix[H - I - 1, W, A], Matrix[H - (I + 1), W, A]] =
    _.asInstanceOf
  given c12[H <: Int, I <: Int, W <: Int, A]: Conversion[Matrix[I + (H - I - 1), W, A], Matrix[H - 1, W, A]] =
    _.asInstanceOf
  given c13[H <: Int, I <: Int, W <: Int, A](using H - I =:= 1): Conversion[Matrix[I, W, A], Matrix[H - 1, W, A]] =
    _.asInstanceOf

  given c4[S <: Int, A](using S =:= 1): Conversion[Vector[1, A], Vector[S, A]] = _.asInstanceOf
  given c5[S <: Int, A]: Conversion[Vector[S - 1 + 1, A], Vector[S, A]]        = _.asInstanceOf

  given c6[S <: Int]: Conversion[S =:= 1, S - 1 =:= 0] = _.asInstanceOf

  given c7[W <: Int]: Conversion[Evidence[W - 1 > 0], Evidence[W > 1]] = _.asInstanceOf

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
