package matrix

import utils.ProductA

import scala.compiletime.ops.any.==

trait Matrix[Weight <: Int & Singleton, Height <: Int & Singleton, +A]:
  inline final def shape: (Weight, Height) = valueOf[Weight] -> valueOf[Height]
  inline final def isSquare: Boolean       = valueOf[Weight == Height]

  def *[A1 >: A, B](scalar: B)(using ProductA[A1, B]): Matrix[Weight, Height, A1]

object Matrix:
  private class Impl[Weight <: Int & Singleton, Height <: Int & Singleton, +A](
    table: Vector[Height, Vector[Weight, A]]
  ) extends Matrix[Weight, Height, A]:
    def *[A1 >: A, B](scalar: B)(using ProductA[A1, B]): Matrix[Weight, Height, A1] = {
      given ProductA[Vector[Weight, A1], B] = _ * _
      Impl[Weight, Height, A1](table * scalar)
    }

    override def toString: String = table.toString
  end Impl

  def apply[Weight <: Int & Singleton, Height <: Int & Singleton, A](
    table: Vector[Height, Vector[Weight, A]]
  ): Matrix[Weight, Height, A] = Impl(table)

@main def test = {
  val matrix: Matrix[3, 4, Int] = Matrix {
    Vector.make[4](
      (
        Vector.make[3]((1, 4, 5)),
        Vector.make[3]((4, 1, 4)),
        Vector.make[3]((0, 1, 4)),
        Vector.make[3]((0, 14, 4)),
      )
    )
  }

  println(s"result = $matrix")
  println(s"shape = ${matrix.shape}")
  println(s"square = ${matrix.isSquare}")
  println(s"* = ${matrix * 100}")
}
