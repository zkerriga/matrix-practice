package matrix.core

import matrix.{Matrix, Vector}
import math.aliases.*
import math.syntax.*
import math.{Zero, One}
import scala.annotation.tailrec

object GaussianElimination {
  import scala.compiletime.ops.int.*

  import matrix.Evidence
  import matrix.lemmas.given

  def l1[W <: Int, A](a: Either[W =:= 1, Vector[W - 1, A]]): Either[W - 1 =:= 0, Vector[W - 1, A]] = a.asInstanceOf
  def l2[W <: Int](a: W =:= 1): W - 1 =:= 0                                                        = a.asInstanceOf
  def l3[W <: Int, A](a: Either[W - 1 =:= 0, Vector[W - 1, A]]): Either[W =:= 1, Vector[W - 1, A]] = a.asInstanceOf

  def emap2[E, A, B, C](aE: Either[E, A], bE: Either[E, B])(f: (A, B) => C): Either[E, C] =
    (aE, bE) match
      case (Right(a), Right(b)) => Right(f(a, b))
      case (Left(error), _)     => Left(error)
      case (_, Left(error))     => Left(error)

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
          case Tail(tail)        => GaussianElimination.desplit(value, l3(tail))

    case class Skip[Size <: Int, A](a: A, next: Node[Size - 1, A]) extends Artificial[Size, A](a, next):
      def divideBy(lead: A)(using Div[A]): Skip[Size, A] = Skip(a / lead, next.divideBy(lead))

    case class Zero[Size <: Int, A](zero: A, next: Node[Size - 1, A]) extends Artificial[Size, A](zero, next):
      def divideBy(lead: A)(using Div[A]): Zero[Size, A] = Zero(zero, next.divideBy(lead))

    case class Tail[Size <: Int, A](tail: Either[Size =:= 0, Vector[Size, A]]) extends Node[Size, A]:
      // def tryToVector: Either[Size =:= 0, Vector[Size, A]] = tail
      def divideBy(lead: A)(using Div[A]): Tail[Size, A] = Tail(tail.map(_.map(_ / lead)))

    def apply[W <: Int, A](e: Either[W =:= 1, Node[W - 1, A]]): Node[W - 1, A] = e match
      case Left(wIs1)  => Tail(Left(l2(wIs1)))
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
          case ZeroColumn => Node.Skip[W, A](baseLead, Node.Tail[W - 1, A](l1(maybeBaseTail)))
          case Node.Tail(maybeDownTail) =>
            val upSubtracted =
              emap2(l1(maybeBaseTail), maybeDownTail) { (baseTail, downTail) =>
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

  def moveNonZeroLeadRowToTop[H <: Int, W <: Int, A: Zero](matrix: Matrix[H, W, A]): Matrix[H, W, A] = {
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
              case Right(tailMatrix) => moving(tailMatrix.asH[H - (I + 1)], skipped.addDown(topVector))
              case _                 => matrix
          else
            (maybeTailMatrix match
              case Right(tailMatrix) => tailMatrix.addTop(skipped).asH[H - 1]
              case Left(is1) =>
                given =:=[H - I, 1] = is1
                skipped.asH[H - 1]
            ).addTop(topVector).asH[H]
        }
        moving[1](tailMatrix, Matrix { Vector.of(topVector) })
      }
  }

  def recursive[H <: Int, W <: Int, A: Div: Mul: Sub: Zero: One](matrix: Matrix[H, W, A]): SubMatrixResult[H, W, A] = {
    val swapped = moveNonZeroLeadRowToTop(matrix)

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
