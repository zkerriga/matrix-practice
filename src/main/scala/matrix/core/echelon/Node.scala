package matrix.core.echelon

import math.aliases.Div
import math.syntax.*
import matrix.Vector
import matrix.core.CatsLikeSyntax.*
import matrix.core.LemmaConversions.given
import matrix.core.MatrixConstructors.desplit
import matrix.lemmas.given

import scala.compiletime.ops.int.*

/**
 * This trait is a representation of [[Vector]], in which some are divided into the following categories for the
 * back-subtraction process in [[matrix.core.GaussianElimination]]:
 *   - [[Node.Tail]] - vector tail, involved in subtraction and division
 *   - [[Node.Skip]] - the element under which there is no leading unit, participates in subtraction and division
 *   - [[Node.Zero]] - the zero element, under which there is a leading unit, does not participate in division and
 *     subtraction
 */
private[core] sealed trait Node[Size <: Int, A]:
  def divideBy(lead: A)(using Div[A]): Node[Size, A]

private[core] object Node:
  sealed trait Processed[Size <: Int, A](value: A, next: Node[Size - 1, A]) extends Node[Size, A]:
    def divideBy(lead: A)(using Div[A]): Node.Processed[Size, A]
    def toVector: Vector[Size, A] =
      next match
        case node: Processed[Size - 1, A] => value +: node.toVector
        case Tail(tail)                   => desplit(value, tail)

  case class Skip[Size <: Int, A](a: A, next: Node[Size - 1, A]) extends Processed[Size, A](a, next):
    def divideBy(lead: A)(using Div[A]): Skip[Size, A] = Skip(a / lead, next.divideBy(lead))

  case class Zero[Size <: Int, A](zero: A, next: Node[Size - 1, A]) extends Processed[Size, A](zero, next):
    def divideBy(lead: A)(using Div[A]): Zero[Size, A] = Zero(zero, next.divideBy(lead))

  case class Tail[Size <: Int, A](tail: Either[Size =:= 0, Vector[Size, A]]) extends Node[Size, A]:
    def divideBy(lead: A)(using Div[A]): Tail[Size, A] = Tail(tail.map(_.map(_ / lead)))

  def apply[W <: Int, A](e: Either[W =:= 1, Node[W - 1, A]]): Node[W - 1, A] = e match
    case Left(wIs1)  => Tail(wIs1.asLeft)
    case Right(node) => node

  def map2[W <: Int, A](base: Node[W, A], down: Node[W, A])(f: (A, A) => A): Node[W, A] =
    base match
      case Skip(a, next) =>
        down match
          case Skip(downA, downNext) => Skip(f(a, downA), map2(next, downNext)(f))
          case _                     => base

      case Zero(zero, next) =>
        down match
          case Zero(_, downNext) => Zero(zero, map2(next, downNext)(f))
          case _                 => base

      case Tail(tail) =>
        down match
          case Tail(downTail) =>
            Tail(Either.map2(tail, downTail) { (tailVector, downVector) => Vector.map2(tailVector, downVector)(f) })
          case _ => base
