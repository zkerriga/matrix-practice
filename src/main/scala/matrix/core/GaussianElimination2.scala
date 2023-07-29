package matrix.core

import matrix.{Matrix, Vector}
import math.aliases.*
import math.syntax.*
import math.{Zero, One}

object GaussianElimination2 {
  import scala.compiletime.ops.int.*

  import matrix.Evidence
  import matrix.lemmas.given
  import experimental.UpSubtraction.{NodeTrap, ZeroColumn, Node}

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

  def subtractBy[A: Mul: Sub](coefficient: A)(x: A, base: A): A = x - base * coefficient

  def desplitTop[H <: Int, W <: Int, A](
    top: Vector[W, A],
    tail: Either[H =:= 1, Matrix[H - 1, W, A]],
  ): Matrix[H, W, A] =
    tail match
      case Right(tailMatrix) => tailMatrix.addTop(top).asH[H]
      case Left(is1) =>
        given =:=[1, H] = is1.flip
        Matrix(Vector.of(top)).asH[H]

  def desplit[Size <: Int, A](lead: A, tail: Either[Size =:= 1, Vector[Size - 1, A]]): Vector[Size, A] =
    tail match
      case Right(tailVector) => (lead +: tailVector).asS[Size]
      case Left(is1) =>
        given =:=[1, Size] = is1.flip
        Vector.of(lead).asS[Size]

  def dropZeroColumn[H <: Int, W <: Int, A](
    topVectorTail: Vector[W - 1, A],
    maybeTailMatrix: Either[H =:= 1, Matrix[H - 1, W, A]],
  ): Matrix[H, W - 1, A] =
    import topVectorTail.sizeEvidence
    desplitTop(
      topVectorTail,
      maybeTailMatrix.map(_.leftTail),
    )

  def onZeroColumn[H <: Int, W <: Int, A: Div: Mul: Sub: Zero: One](
    height: H,
    maybeRightMatrixToProcess: Either[W =:= 1, Matrix[H, W - 1, A]],
  )(using Evidence[H > 0]): SubMatrixResult[H, W, A] =
    val zero       = Zero.of[A]
    val zeroColumn = Vector.fill(height)(zero)
    maybeRightMatrixToProcess match
      case Right(rightMatrixToProcess) =>
        val SubMatrixResult(rightMatrix, toSubtract) = recursive(rightMatrixToProcess)
        SubMatrixResult(rightMatrix.addLeft(zeroColumn).asW[W], NodeTrap.Next(ZeroColumn, toSubtract))
      case Left(wIs1) =>
        given =:=[1, W] = wIs1.flip
        SubMatrixResult(
          Matrix(zeroColumn.map(zero => Vector.of[A](zero).asS[W])),
          NodeTrap.First(ZeroColumn),
        )

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
        import topVectorTail.sizeEvidence
        val subtractedMatrix = tailMatrix.mapRows { tailMatrixRow =>
          val rowLead                   = tailMatrixRow.head
          val rowTail: Vector[W - 1, A] = tailMatrixRow.tail
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

    val topVector: Vector[W, A] = (One.of[A] +: subtracted).asS[W]
    val zeroedDownMatrix        = downRightMatrix.mapRows(Zero.of[A] +: _).asW[W]

    SubMatrixResult[H, W, A](
      zeroedDownMatrix.addTop(topVector).asH[H],
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
          subMatrix = desplitTop((One.of[A] +: dividedTail).asS[W], Left(hIs1)),
          toSubtractAbove = NodeTrap.First(Node.Tail(Right(dividedTail))),
        )
      case OnlyLeft(zeroHeight, ev, wIs1) =>
        val zero                 = Zero.of[A]
        val column: Vector[H, A] = (One.of[A] +: Vector.fill[H - 1, A](zeroHeight)(zero)(using ev)).asS[H]
        val matrix               = Matrix(Vector.of(column)).transpose.asW[W](using wIs1.flip)
        SubMatrixResult[H, W, A](
          subMatrix = matrix,
          toSubtractAbove = NodeTrap.First(Node.Tail(Left(l2(wIs1)))),
        )
      case OnlyLead(hIs1, wIs1) =>
        SubMatrixResult[H, W, A](
          subMatrix = Matrix(Vector.of(Vector.of(One.of[A]))).asH[H](using hIs1.flip).asW[W](using wIs1.flip),
          toSubtractAbove = NodeTrap.First(Node.Tail(Left(l2(wIs1)))),
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
        maybeTopVectorTail.map(dropZeroColumn(_, maybeMatrixTail)),
      ).asW[W]
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
