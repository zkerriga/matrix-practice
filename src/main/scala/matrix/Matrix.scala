package matrix

import utils.{HMul, Semigroup}

import scala.compiletime.ops.boolean.*
import scala.compiletime.ops.int.*

trait Matrix[Weight <: Int, Height <: Int, +A](weight: Weight, height: Height)(using
  Evidence[Weight > 0],
  Evidence[Height > 0],
):
  final def shape: (Weight, Height) = weight -> height
  final def isSquare: Boolean       = weight == height

  def apply[Row <: Int & Singleton, Column <: Int & Singleton](row: Row, column: Column)(using
    Evidence[Row >= 0 && Row < Height],
    Evidence[Column >= 0 && Column < Weight],
  ): A

  def *[B, C](scalar: B)(using HMul[A, B, C]): Matrix[Weight, Height, C]
  def +[A1 >: A: Semigroup](other: Matrix[Weight, Height, A1]): Matrix[Weight, Height, A1]

  override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match
    case other: Matrix[Weight @unchecked, Height @unchecked, A @unchecked] =>
      (shape == other.shape) && (0 until height).forall { y =>
        given Evidence[y.type >= 0 && y.type < Height] = guaranteed
        (0 until weight).forall { x =>
          given Evidence[x.type >= 0 && x.type < Weight] = guaranteed
          apply(y, x) == other.apply(y, x)
        }
      }
    case _ => false
  )

object Matrix:
  private class Impl[Weight <: Int, Height <: Int, +A](
    weight: Weight,
    height: Height,
    table: Vector[Height, Vector[Weight, A]],
  )(using Evidence[Weight > 0], Evidence[Height > 0])
      extends Matrix[Weight, Height, A](weight, height):
    def apply[Row <: Int & Singleton, Column <: Int & Singleton](row: Row, column: Column)(using
      Evidence[Row >= 0 && Row < Height],
      Evidence[Column >= 0 && Column < Weight],
    ): A = table.apply[Row](row).apply[Column](column)

    def *[B, C](scalar: B)(using HMul[A, B, C]): Matrix[Weight, Height, C] =
      given HMul[Vector[Weight, A], B, Vector[Weight, C]] = _ * _
      Impl(weight, height, table * scalar)

    def +[A1 >: A: Semigroup](other: Matrix[Weight, Height, A1]): Matrix[Weight, Height, A1] =
      val combinedTable =
        Vector.tabulate[Height, Vector[Weight, A1]](height) { y =>
          Vector.tabulate[Weight, A1](weight) { x =>
            apply(y, x) |+| other(y, x)
          }
        }
      Impl(weight, height, combinedTable)

    override def toString: String = table.toString

  def apply[Weight <: Int, Height <: Int, A](table: Vector[Height, Vector[Weight, A]])(using
    ValueOf[Weight],
    ValueOf[Height],
    Evidence[Weight > 0],
    Evidence[Height > 0], // todo: can we take the evidence from the table?
  ): Matrix[Weight, Height, A] = Impl(valueOf, valueOf, table)
