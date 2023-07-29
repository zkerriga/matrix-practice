package matrix.core

import matrix.{Matrix, Vector}
import math.aliases.*
import math.syntax.*
import math.{Zero, One}

object GaussianElimination2 {
  import scala.compiletime.ops.int.*

  import matrix.Evidence
  import matrix.lemmas.given
  import experimental.UpSubtraction.{NodeTrap, ZeroLine, Node, leftMap}

  import experimental.UpSubtraction.process

  def l1[W <: Int, A](a: Either[W =:= 1, Vector[W - 1, A]]): Either[W - 1 =:= 0, Vector[W - 1, A]] = a.asInstanceOf
  def l2[W <: Int](a: W =:= 1): W - 1 =:= 0                                                        = a.asInstanceOf
  def l3[W <: Int, A](a: Either[W - 1 =:= 0, Vector[W - 1, A]]): Either[W =:= 1, Vector[W - 1, A]] = a.asInstanceOf

  extension [H <: Int, W <: Int, A, F[_ <: Int, _ <: Int, _]](fa: F[H, W, A])
    def asH[H2 <: Int](using H =:= H2): F[H2, W, A] =
      summon[H =:= H2].liftCo[[h] =>> F[h & Int, W, A]](fa)
    def asW[W2 <: Int](using W =:= W2): F[H, W2, A] =
      summon[W =:= W2].liftCo[[w] =>> F[H, w & Int, A]](fa)

  extension [S <: Int, A, F[_ <: Int, _]](fa: F[S, A])
    def asS[S2 <: Int](using S =:= S2): F[S2, A] =
      summon[S =:= S2].liftCo[[s] =>> F[s & Int, A]](fa)

  case class SubMatrixResult[H <: Int, W <: Int, A](
    subMatrix: Matrix[H, W, A],
    toSubtractAbove: NodeTrap[W - 1, A],
  )

  def onZeroColumn[H <: Int, W <: Int, A: Div: Mul: Sub: Zero: One](
    height: H,
    maybeRightMatrixToProcess: Either[W =:= 1, Matrix[H, W - 1, A]],
  )(using Evidence[H > 0]): SubMatrixResult[H, W, A] =
    val zero                     = Zero.of[A]
    val zeroColumn: Vector[H, A] = Vector.fill(height)(zero)
    maybeRightMatrixToProcess match
      case Right(rightMatrixToProcess) =>
        val SubMatrixResult(rightMatrix, toSubtract) = recursive(rightMatrixToProcess)
        SubMatrixResult(rightMatrix.addLeft(zeroColumn).asW[W], NodeTrap.Next(ZeroLine, toSubtract))
      case Left(is1) =>
        val wIs1: Vector[1, A] =:= Vector[W, A] = is1.liftContra[[s] =>> Vector[s & Int, A]] // todo: refactor
        SubMatrixResult(
          Matrix(zeroColumn.map(zero => wIs1(Vector.of[A](zero)))),
          NodeTrap.First(ZeroLine),
        )

  import GaussianElimination.{desplit, desplitTop, subtractBy}

  enum LeftSubtractDown[H <: Int, W <: Int]:
    case H1(hIs1: H =:= 1)
    case W1(wIs1: W =:= 1, ev: Evidence[H - 1 > 0])
    case HW1(hIs1: H =:= 1, wIs1: W =:= 1)

  def subtractDown[H <: Int, W <: Int, A: Div: Mul: Sub: Zero](
    topVectorLead: A,
    maybeTopVectorTail: Either[W =:= 1, Vector[W - 1, A]],
    maybeTailMatrix: Either[H =:= 1, Matrix[H - 1, W, A]],
  ): Either[LeftSubtractDown[H, W], Matrix[H - 1, W - 1, A]] =
    maybeTopVectorTail match
      case Right(topVectorTail) =>
        maybeTailMatrix
          .map { tailMatrix =>
            import topVectorTail.sizeEvidence
            tailMatrix.mapRows { tailMatrixRow =>
              val rowLead                   = tailMatrixRow.head
              val rowTail: Vector[W - 1, A] = tailMatrixRow.tail
              if rowLead == Zero.of[A] then rowTail
              else Vector.map2(rowTail, topVectorTail)(subtractBy(rowLead / topVectorLead))
            }
          }.leftMap(LeftSubtractDown.H1(_))
      case Left(wIs1) =>
        Left {
          maybeTailMatrix match
            case Left(hIs1)    => LeftSubtractDown.HW1(hIs1, wIs1)
            case Right(matrix) => LeftSubtractDown.W1(wIs1, matrix.heightEvidence)
        }

  def onDownSubtraction[H <: Int, W <: Int, A: Div: Mul: Sub: Zero: One](
    height: H,
    topLead: A,
    maybeTopTail: Either[W =:= 1, Vector[W - 1, A]],
    maybeDownRightMatrixToProcess: Either[LeftSubtractDown[H, W], Matrix[H - 1, W - 1, A]],
  ): SubMatrixResult[H, W, A] =
    maybeDownRightMatrixToProcess match
      case Left(evidence) =>
        evidence match
          case LeftSubtractDown.H1(hIs1) =>
            val maybeDividedTail = maybeTopTail.map(_.map(_ / topLead))
            SubMatrixResult[H, W, A](
              subMatrix = desplitTop(desplit(One.of[A], maybeDividedTail), Left(hIs1)),
              toSubtractAbove = NodeTrap.First(Node.Tail(l1(maybeDividedTail))),
            )
          case LeftSubtractDown.W1(wIs1, ev) =>
            val zero = Zero.of[A]
            val column: Vector[H, A] =
              (One.of[A] +: Vector.fill[H - 1, A]((height - 1).asInstanceOf)(zero)(using ev)).asS[H]
            val matrix = Matrix(wIs1.liftContra[[s] =>> Vector[s & Int, Vector[H, A]]](Vector.of(column))).transpose
            SubMatrixResult[H, W, A](
              subMatrix = matrix,
              toSubtractAbove = NodeTrap.First(Node.Tail(Left(l2(wIs1)))),
            )
          case LeftSubtractDown.HW1(hIs1, wIs1) =>
            val matrix11 = Matrix(Vector.of(Vector.of(One.of[A])))
            val matrixH1 = hIs1.liftContra[[h] =>> Matrix[h & Int, 1, A]](matrix11)
            val matrixHW = wIs1.liftContra[[w] =>> Matrix[H, w & Int, A]](matrixH1)
            SubMatrixResult[H, W, A](
              subMatrix = matrixHW,
              toSubtractAbove = NodeTrap.First(Node.Tail(Left(l2(wIs1)))),
            )

      case Right(downRightMatrixToProcess) =>
        val SubMatrixResult(downRightMatrix, toSubtract) = recursive(downRightMatrixToProcess)

        val subtractedNode: Either[W =:= 1, Node.Artificial[W - 1, A]] = maybeTopTail.map { topTail =>
          process(topTail, toSubtract).divideBy(topLead)
        }

        val subtracted: Either[W =:= 1, Vector[W - 1, A]] = subtractedNode.flatMap { node =>
          l3(node.toVector)
        }

        val topVector: Vector[W, A] = desplit(One.of[A], subtracted)
        val zeroedDownMatrix        = downRightMatrix.mapRows(Zero.of[A] +: _).asW[W]

        val toSubtractAbove: NodeTrap[W - 1, A] = subtractedNode match
          case Left(wIs1)  => NodeTrap.First(Node.Tail(Left(l2(wIs1))))
          case Right(node) => NodeTrap.Next(node, toSubtract)

        SubMatrixResult[H, W, A](
          zeroedDownMatrix.addTop(topVector).asH[H],
          toSubtractAbove,
        )

  def recursive[H <: Int, W <: Int, A: Div: Mul: Sub: Zero: One](matrix: Matrix[H, W, A]): SubMatrixResult[H, W, A] = {
    val swapped = GaussianElimination.moveNonZeroLeadRowToTop(matrix)

    val topVector          = swapped.topRow
    val maybeMatrixTail    = swapped.topTail
    val topVectorLead      = topVector.head
    val maybeTopVectorTail = topVector.tail

    if topVectorLead == Zero.of[A] then
      import matrix.heightEvidence
      onZeroColumn(
        matrix.height,
        maybeTopVectorTail.map(GaussianElimination.dropZeroColumn(_, maybeMatrixTail)),
      ).asW[W]
    else
      onDownSubtraction(
        matrix.height,
        topVectorLead,
        maybeTopVectorTail,
        subtractDown(topVectorLead, maybeTopVectorTail, maybeMatrixTail),
      )
  }

  def on[Height <: Int, Width <: Int, A: Div: Mul: Sub: Zero: One](
    matrix: Matrix[Height, Width, A]
  ): Matrix[Height, Width, A] = recursive(matrix).subMatrix
}
