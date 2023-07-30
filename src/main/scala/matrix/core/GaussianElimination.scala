package matrix.core

import matrix.{Matrix, Vector}
import math.aliases.*
import math.{Zero, One}
import math.syntax.*
import scala.annotation.tailrec
import scala.compiletime.ops.int.*

import matrix.Evidence
import matrix.lemmas.given

object GaussianElimination {
  private object lemmas {
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
  }

  def emap2[E, A, B, C](aE: Either[E, A], bE: Either[E, B])(f: (A, B) => C): Either[E, C] =
    (aE, bE) match
      case (Right(a), Right(b)) => Right(f(a, b))
      case (Left(error), _)     => Left(error)
      case (_, Left(error))     => Left(error)

  case class SubMatrixResult[H <: Int, W <: Int, A](
    subMatrix: Matrix[H, W, A],
    toSubtractAbove: NodeTrap[W - 1, A],
  )

  def subtractBy[A: Mul: Sub](coefficient: A)(x: A, base: A): A = x - base * coefficient

  import lemmas.given

  def desplitTop[H <: Int, W <: Int, A](
    top: Vector[W, A],
    tail: Either[H =:= 1, Matrix[H - 1, W, A]],
  ): Matrix[H, W, A] =
    tail match
      case Right(tailMatrix) => tailMatrix.addTop(top)
      case Left(is1) =>
        given =:=[H, 1] = is1
        Matrix(Vector.one(top))

  def desplit[Size <: Int, A](lead: A, tail: Either[Size =:= 1, Vector[Size - 1, A]]): Vector[Size, A] =
    tail match
      case Right(tailVector) => lead +: tailVector
      case Left(is1) =>
        given =:=[Size, 1] = is1 // todo: using close?
        Vector.one(lead)

  //

  sealed trait Node[Size <: Int, A]:
    def divideBy(lead: A)(using Div[A]): Node[Size, A]

  object Node:
    sealed trait Artificial[Size <: Int, A](value: A, next: Node[Size - 1, A]) extends Node[Size, A]:
      def divideBy(lead: A)(using Div[A]): Node.Artificial[Size, A]
      def toVector: Vector[Size, A] =
        next match
          case node @ Skip(_, _) => (value +: node.toVector).asInstanceOf[Vector[Size, A]] // todo: fix
          case node @ Zero(_, _) => (value +: node.toVector).asInstanceOf[Vector[Size, A]] // todo: fix
          case Tail(tail)        => desplit(value, tail)

    case class Skip[Size <: Int, A](a: A, next: Node[Size - 1, A]) extends Artificial[Size, A](a, next):
      def divideBy(lead: A)(using Div[A]): Skip[Size, A] = Skip(a / lead, next.divideBy(lead))

    case class Zero[Size <: Int, A](zero: A, next: Node[Size - 1, A]) extends Artificial[Size, A](zero, next):
      def divideBy(lead: A)(using Div[A]): Zero[Size, A] = Zero(zero, next.divideBy(lead))

    case class Tail[Size <: Int, A](tail: Either[Size =:= 0, Vector[Size, A]]) extends Node[Size, A]:
      def divideBy(lead: A)(using Div[A]): Tail[Size, A] = Tail(tail.map(_.map(_ / lead)))

    def apply[W <: Int, A](e: Either[W =:= 1, Node[W - 1, A]]): Node[W - 1, A] = e match
      case Left(wIs1)  => Tail(Left(wIs1))
      case Right(node) => node

    // todo: is it possible to skip => base cases?
    def map2[W <: Int, A](base: Node[W, A], down: Node[W, A])(f: (A, A) => A): Node[W, A] =
      base match
        case Skip(a, next) =>
          down match
            case Skip(downA, downNext) => Skip(f(a, downA), map2(next, downNext)(f))
            case Zero(zero, next)      => base
            case Tail(tail)            => base

        case Zero(zero, next) =>
          down match
            case Zero(_, downNext) => Zero(zero, map2(next, downNext)(f))
            case Skip(a, next)     => base
            case Tail(tail)        => base

        case Tail(tail) =>
          down match
            case Tail(downTail) =>
              Tail(emap2(tail, downTail) { (tailVector, downVector) => Vector.map2(tailVector, downVector)(f) })
            case Skip(a, next)    => base
            case Zero(zero, next) => base

  case object ZeroColumn
  type ZeroColumn = ZeroColumn.type

  enum NodeTrap[W <: Int, A]:
    case First(down: ZeroColumn | Node.Tail[W, A])
    case Next(down: ZeroColumn | Node.Artificial[W, A], next: NodeTrap[W - 1, A])

  def process[W <: Int, A: Mul: Sub: Zero](
    base: Vector[W, A],
    trap: NodeTrap[W - 1, A],
  ): Node.Artificial[W, A] =
    val baseLead: A   = base.head
    val maybeBaseTail = base.tail
    trap match
      case NodeTrap.First(down) =>
        down match
          case ZeroColumn => Node.Skip[W, A](baseLead, Node.Tail[W - 1, A](maybeBaseTail))
          case Node.Tail(maybeDownTail) =>
            val upSubtracted: Either[W - 1 =:= 0, Vector[W - 1, A]] =
              emap2[W - 1 =:= 0, Vector[W - 1, A], Vector[W - 1, A], Vector[W - 1, A]](maybeBaseTail, maybeDownTail) {
                (baseTail, downTail) =>
                  Vector.map2(baseTail, downTail)(subtractBy(baseLead))
              }
            Node.Zero[W, A](Zero.of[A], Node.Tail[W - 1, A](upSubtracted))

      case NodeTrap.Next(down, nextTrap) =>
        val maybeProcessed = maybeBaseTail.map { baseTail => process(baseTail, nextTrap) }
        def onArtificial(node: Node.Artificial[W - 1, A]): Node.Artificial[W, A] =
          Node.Zero[W, A](Zero.of[A], Node(maybeProcessed.map(Node.map2(_, node)(subtractBy(baseLead)))))
        down match
          case ZeroColumn             => Node.Skip[W, A](baseLead, Node(maybeProcessed))
          case node @ Node.Skip(_, _) => onArtificial(node)
          case node @ Node.Zero(_, _) => onArtificial(node)

  //

  enum SubtractedDown[H <: Int, W <: Int, A]:
    case ToProcess(topTail: Vector[W - 1, A], downRight: Matrix[H - 1, W - 1, A])
    case OnlyTop(topTail: Vector[W - 1, A], hIs1: H =:= 1)
    case OnlyLeft(zeroHeight: H - 1, ev: Evidence[H - 1 > 0], wIs1: W =:= 1)
    case OnlyLead(hIs1: H =:= 1, wIs1: W =:= 1)

  def subtractDown[H <: Int, W <: Int, A: Div: Mul: Sub: Zero](
    topVectorLead: A,
    maybeTopVectorTail: Either[W =:= 1, Vector[W - 1, A]],
    maybeTailMatrix: Either[H =:= 1, Matrix[H - 1, W, A]],
  ): SubtractedDown[H, W, A] =
    import SubtractedDown.*
    (maybeTopVectorTail, maybeTailMatrix) match
      case (Right(topVectorTail), Right(tailMatrix)) =>
        val subtractedMatrix = tailMatrix.mapRows { tailMatrixRow =>
          val rowLead                   = tailMatrixRow.head
          val rowTail: Vector[W - 1, A] = tailMatrixRow.tail(using topVectorTail.sizeEvidence)
          if rowLead == Zero.of[A] then rowTail
          else Vector.map2(rowTail, topVectorTail)(subtractBy(rowLead / topVectorLead))
        }
        ToProcess(topVectorTail, subtractedMatrix)
      case (Right(topVectorTail), Left(hIs1)) => OnlyTop(topVectorTail, hIs1)
      case (Left(wIs1), Right(tailMatrix))    => OnlyLeft(tailMatrix.height, tailMatrix.heightEvidence, wIs1)
      case (Left(wIs1), Left(hIs1))           => OnlyLead(hIs1, wIs1)

  def onDownRightMatrix[H <: Int, W <: Int, A: Div: Mul: Sub: Zero: One](
    topLead: A,
    topTail: Vector[W - 1, A],
    downRightMatrixToProcess: Matrix[H - 1, W - 1, A],
  ): SubMatrixResult[H, W, A] =
    val SubMatrixResult(downRightMatrix, toSubtract) = recursive(downRightMatrixToProcess)

    val subtractedNode: Node.Artificial[W - 1, A] =
      process(topTail, toSubtract).divideBy(topLead)

    val subtracted: Vector[W - 1, A] = subtractedNode.toVector

    val topVector: Vector[W, A]               = One.of[A] +: subtracted
    val zeroedDownMatrix: Matrix[H - 1, W, A] = downRightMatrix.mapRows(Zero.of[A] +: _)

    SubMatrixResult[H, W, A](
      zeroedDownMatrix.addTop(topVector),
      NodeTrap.Next(subtractedNode, toSubtract),
    )

  def onDownSubtraction[H <: Int, W <: Int, A: Div: Mul: Sub: Zero: One](
    topLead: A,
    toProcess: SubtractedDown[H, W, A],
  ): SubMatrixResult[H, W, A] =
    import SubtractedDown.*
    toProcess match
      case ToProcess(topTail, downRight) =>
        onDownRightMatrix(topLead, topTail, downRight)
      case OnlyTop(topTail, hIs1) =>
        val dividedTail = topTail.map(_ / topLead)
        SubMatrixResult[H, W, A](
          subMatrix = desplitTop(One.of[A] +: dividedTail, Left(hIs1)),
          toSubtractAbove = NodeTrap.First(Node.Tail(Right(dividedTail))),
        )
      case OnlyLeft(zeroHeight, ev, wIs1) =>
        given =:=[W, 1]             = wIs1
        val zero                    = Zero.of[A]
        val column: Vector[H, A]    = One.of[A] +: Vector.fill[H - 1, A](zeroHeight)(zero)(using ev)
        val matrix: Matrix[H, W, A] = Matrix(Vector.one(column)).transpose
        SubMatrixResult[H, W, A](
          subMatrix = matrix,
          toSubtractAbove = NodeTrap.First(Node.Tail(Left(wIs1))),
        )
      case OnlyLead(hIs1, wIs1) =>
        given =:=[H, 1] = hIs1
        given =:=[W, 1] = wIs1
        SubMatrixResult[H, W, A](
          subMatrix = Matrix[1, 1, A](Vector.one(Vector.one(One.of[A]))),
          toSubtractAbove = NodeTrap.First(Node.Tail(Left(wIs1))),
        )

  private def dropZeroColumn[H <: Int, W <: Int, A](
    topVectorTail: Vector[W - 1, A],
    maybeTailMatrix: Either[H =:= 1, Matrix[H - 1, W, A]],
  ): Matrix[H, W - 1, A] =
    desplitTop(
      topVectorTail,
      maybeTailMatrix.map(_.leftTail(using topVectorTail.sizeEvidence)),
    )

  private def onZeroColumn[H <: Int, W <: Int, A: Div: Mul: Sub: Zero: One](
    height: H,
    maybeRightMatrixToProcess: Either[W =:= 1, Matrix[H, W - 1, A]],
  )(using Evidence[H > 0]): SubMatrixResult[H, W, A] =
    val zero       = Zero.of[A]
    val zeroColumn = Vector.fill(height)(zero)
    maybeRightMatrixToProcess match
      case Right(rightMatrixToProcess) =>
        val SubMatrixResult(rightMatrix, toSubtract) = recursive(rightMatrixToProcess)
        SubMatrixResult(rightMatrix.addLeft(zeroColumn), NodeTrap.Next(ZeroColumn, toSubtract))
      case Left(wIs1) =>
        given =:=[W, 1] = wIs1
        SubMatrixResult(
          Matrix(zeroColumn.map(zero => Vector.one[A](zero))),
          NodeTrap.First(ZeroColumn),
        )

  def moveNonZeroLeadRowToTop[H <: Int, W <: Int, A: Zero](matrix: Matrix[H, W, A]): Matrix[H, W, A] =
    val topVector = matrix.topRow
    if topVector.head != Zero.of[A] then matrix
    else
      matrix.topTail.toOption.fold(matrix) { tailMatrix =>
        @tailrec
        def moving[I <: Int](left: Matrix[H - I, W, A], skipped: Matrix[I, W, A]): Matrix[H, W, A] = {
          val topVector       = left.topRow
          val maybeTailMatrix = left.topTail
          if topVector.head == Zero.of[A] then
            maybeTailMatrix match
              case Right(tailMatrix) => moving(tailMatrix, skipped.addDown(topVector))
              case _                 => matrix
          else
            val result: Matrix[H - 1, W, A] = maybeTailMatrix match
              case Right(tailMatrix) => tailMatrix.addTop(skipped)
              case Left(hIs1) =>
                given =:=[H - I, 1] = hIs1
                skipped
            result.addTop(topVector)
        }
        moving[1](tailMatrix, Matrix { Vector.one(topVector) })
      }

  private def recursive[H <: Int, W <: Int, A: Div: Mul: Sub: Zero: One](
    matrix: Matrix[H, W, A]
  ): SubMatrixResult[H, W, A] = {
    val swapped = moveNonZeroLeadRowToTop(matrix)

    val topVector          = swapped.topRow
    val maybeMatrixTail    = swapped.topTail
    val topVectorLead      = topVector.head
    val maybeTopVectorTail = topVector.tail

    if topVectorLead == Zero.of[A] then
      import matrix.heightEvidence
      onZeroColumn(
        matrix.height,
        maybeTopVectorTail.map { vector =>
          swapped.leftTail(using vector.sizeEvidence)
        },
      )
    else
      onDownSubtraction(
        topVectorLead,
        subtractDown(topVectorLead, maybeTopVectorTail, maybeMatrixTail),
      )
  }

  def on[Height <: Int, Width <: Int, A: Div: Mul: Sub: Zero: One](
    matrix: Matrix[Height, Width, A]
  ): Matrix[Height, Width, A] = recursive(matrix).subMatrix
}
