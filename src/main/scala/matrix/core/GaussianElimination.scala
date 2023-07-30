package matrix.core

import math.aliases.*
import math.{Zero, One}
import math.syntax.*
import matrix.{Evidence, Matrix, Vector}
import matrix.core.CatsLikeSyntax.*
import matrix.core.MatrixConversions.{*, given}

import scala.annotation.tailrec
import scala.compiletime.ops.int.*

object GaussianElimination {
  private case class SubMatrixResult[H <: Int, W <: Int, A](
    subMatrix: Matrix[H, W, A],
    toSubtractAbove: Trapezoid[W - 1, A],
  )

  private def subtractBy[A: Mul: Sub](coefficient: A)(x: A, base: A): A =
    x - base * coefficient

  private def subtractUp[W <: Int, A: Mul: Sub: Zero](
    base: Vector[W, A],
    toSubtract: Trapezoid[W - 1, A],
  ): Node.Artificial[W, A] =
    val baseLead: A   = base.head
    val maybeBaseTail = base.tail
    toSubtract match
      case Trapezoid.First(down) =>
        down match
          case Trapezoid.ZeroColumn => Node.Skip[W, A](baseLead, Node.Tail[W - 1, A](maybeBaseTail))
          case Node.Tail(maybeDownTail) =>
            val upSubtracted: Either[W - 1 =:= 0, Vector[W - 1, A]] =
              Either.map2[W - 1 =:= 0, Vector[W - 1, A], Vector[W - 1, A], Vector[W - 1, A]](
                maybeBaseTail,
                maybeDownTail,
              ) { (baseTail, downTail) =>
                Vector.map2(baseTail, downTail)(subtractBy(baseLead))
              }
            Node.Zero[W, A](Zero.of[A], Node.Tail[W - 1, A](upSubtracted))

      case Trapezoid.Next(down, nextTrap) =>
        val maybeProcessed = maybeBaseTail.map { baseTail => subtractUp(baseTail, nextTrap) }
        def onArtificial(node: Node.Artificial[W - 1, A]): Node.Artificial[W, A] =
          Node.Zero[W, A](Zero.of[A], Node(maybeProcessed.map(Node.map2(_, node)(subtractBy(baseLead)))))
        down match
          case Trapezoid.ZeroColumn   => Node.Skip[W, A](baseLead, Node(maybeProcessed))
          case node @ Node.Skip(_, _) => onArtificial(node)
          case node @ Node.Zero(_, _) => onArtificial(node)

  private enum SubtractedDown[H <: Int, W <: Int, A]:
    case ToProcess(topTail: Vector[W - 1, A], downRight: Matrix[H - 1, W - 1, A])
    case OnlyTop(topTail: Vector[W - 1, A], hIs1: H =:= 1)
    case OnlyLeft(zeroHeight: H - 1, ev: Evidence[H - 1 > 0], wIs1: W =:= 1)
    case OnlyLead(hIs1: H =:= 1, wIs1: W =:= 1)

  private def subtractDown[H <: Int, W <: Int, A: Div: Mul: Sub: Zero](
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

  private def onDownRightMatrix[H <: Int, W <: Int, A: Div: Mul: Sub: Zero: One](
    topLead: A,
    topTail: Vector[W - 1, A],
    downRightMatrixToProcess: Matrix[H - 1, W - 1, A],
  ): SubMatrixResult[H, W, A] =
    val SubMatrixResult(downRightMatrix, toSubtract) = recursive(downRightMatrixToProcess)

    val subtracted: Node.Artificial[W - 1, A] = subtractUp(topTail, toSubtract).divideBy(topLead)

    val topVector: Vector[W, A]               = One.of[A] +: subtracted.toVector
    val zeroedDownMatrix: Matrix[H - 1, W, A] = downRightMatrix.mapRows(Zero.of[A] +: _)

    SubMatrixResult[H, W, A](
      zeroedDownMatrix.addTop(topVector),
      Trapezoid.Next(subtracted, toSubtract),
    )

  private def onDownSubtraction[H <: Int, W <: Int, A: Div: Mul: Sub: Zero: One](
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
          considering(hIs1) { Matrix(Vector.one[Vector[W, A]](One.of[A] +: dividedTail)) },
          Trapezoid.First(Node.Tail(dividedTail.asRight)),
        )
      case OnlyLeft(height, ev, wIs1) =>
        val column: Vector[H, A]    = One.of[A] +: Vector.fill[H - 1, A](height)(Zero.of[A])(using ev)
        val matrix: Matrix[H, W, A] = considering(wIs1) { Matrix(Vector.one(column)).transpose }
        SubMatrixResult[H, W, A](
          matrix,
          Trapezoid.First(Node.Tail(wIs1.asLeft)),
        )
      case OnlyLead(hIs1, wIs1) =>
        SubMatrixResult[H, W, A](
          considering(hIs1, wIs1) { Matrix[H, W, A](Vector.one[Vector[W, A]](Vector.one(One.of[A]))) },
          Trapezoid.First(Node.Tail(wIs1.asLeft)),
        )

  private def onZeroColumn[H <: Int, W <: Int, A: Div: Mul: Sub: Zero: One](
    height: H,
    maybeRightMatrixToProcess: Either[W =:= 1, Matrix[H, W - 1, A]],
  )(using Evidence[H > 0]): SubMatrixResult[H, W, A] =
    val zeroColumn = Vector.fill(height)(Zero.of[A])
    maybeRightMatrixToProcess match
      case Right(rightMatrixToProcess) =>
        val SubMatrixResult(rightMatrix, toSubtract) = recursive(rightMatrixToProcess)
        SubMatrixResult(rightMatrix.addLeft(zeroColumn), Trapezoid.Next(Trapezoid.ZeroColumn, toSubtract))
      case Left(wIs1) =>
        SubMatrixResult(
          considering(wIs1) { Matrix(zeroColumn.map(zero => Vector.one[A](zero))) },
          Trapezoid.First(Trapezoid.ZeroColumn),
        )

  private def moveNonZeroLeadRowToTop[H <: Int, W <: Int, A: Zero](matrix: Matrix[H, W, A]): Matrix[H, W, A] =
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
            val rest: Matrix[H - 1, W, A] =
              maybeTailMatrix match
                case Right(tailMatrix) => tailMatrix.addTop(skipped)
                case Left(hIs1)        => considering(hIs1) { skipped }
            rest.addTop(topVector)
        }
        moving[1](tailMatrix, Matrix(Vector.one(topVector)))
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
