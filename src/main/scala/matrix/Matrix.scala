package matrix

import utils.Product

import scala.compiletime.ops.any.==

trait Matrix[Weight <: Int, Height <: Int, +A](weight: Weight, height: Height):
  final def shape: (Weight, Height) = weight -> height
  final def isSquare: Boolean       = weight == height

  def *[B, C](scalar: B)(using Product[A, B, C]): Matrix[Weight, Height, C]

object Matrix:
  private class Impl[Weight <: Int, Height <: Int, +A](
    weight: Weight,
    height: Height,
    table: Vector[Height, Vector[Weight, A]],
  ) extends Matrix[Weight, Height, A](weight, height):
    def *[B, C](scalar: B)(using Product[A, B, C]): Matrix[Weight, Height, C] = {
      given Product[Vector[Weight, A], B, Vector[Weight, C]] = _ * _
      Impl[Weight, Height, C](weight, height, table * scalar)
    }

    override def toString: String = table.toString
  end Impl

  def apply[Weight <: Int, Height <: Int, A](
    table: Vector[Height, Vector[Weight, A]]
  )(using ValueOf[Weight], ValueOf[Height]): Matrix[Weight, Height, A] =
    Impl(valueOf, valueOf, table)

@main def test = {
  val matrix: Matrix[3, 4, Int] = Matrix {
    Vector.of(
      Vector.of(1, 4, 5),
      Vector.of(4, 1, 4),
      Vector.of(0, 1, 4),
      Vector.of(0, 14, 4),
    )
  }

  println(s"result = $matrix")
  println(s"shape = ${matrix.shape}")
  println(s"square = ${matrix.isSquare}")
  println(s"* = ${matrix * 100}")
}
