package matrix.core

import math.{Zero, One}
import math.aliases.*
import math.syntax.*
import matrix.lemmas.given

import scala.collection.immutable.Vector as StdVector
import matrix.{Evidence, Matrix, Vector}

import scala.annotation.tailrec
import scala.compiletime.ops.int.{+, -, >}

private[matrix] object GaussianElimination:
  type NonEmptyList[+A] = List[A]
  val NonEmptyList = List

  case class SubMatrixResult[H <: Int, W <: Int, +A](
    subMatrix: Matrix[H, W, A],
    // todo: vectors or lists?
    toSubtractAbove: StdVector[NonEmptyList[Option[Vector[_, A]]]],
  )

  // todo: think, where the function should be
  def map2[A, B, C](optionA: Option[A], optionB: Option[B])(f: (A, B) => C): Option[C] =
    for
      a <- optionA
      b <- optionB
    yield f(a, b)

  // todo: think, where the function should be
  def map2[A, B, C](listA: NonEmptyList[A], listB: NonEmptyList[B])(f: (A, B) => C): NonEmptyList[C] =
    listA.lazyZip(listB).map { case (a, b) => f(a, b) }

  extension [H <: Int, W <: Int, A, F[_ <: Int, _ <: Int, _]](fa: F[H, W, A])
    def asH[H2 <: Int](using H =:= H2): F[H2, W, A] =
      summon[H =:= H2].liftCo[[h] =>> F[h & Int, W, A]](fa)
    def asW[W2 <: Int](using W =:= W2): F[H, W2, A] =
      summon[W =:= W2].liftCo[[w] =>> F[H, w & Int, A]](fa)

  extension [S <: Int, A, F[_ <: Int, _]](fa: F[S, A])
    def asS[S2 <: Int](using S =:= S2): F[S2, A] =
      summon[S =:= S2].liftCo[[s] =>> F[s & Int, A]](fa)

  given [A, Size <: Int](using Size =:= 1): Conversion[Vector[1, A], Vector[Size, A]] =
    v1 => summon[Size =:= 1].liftContra[[s] =>> Vector[s & Int, A]](v1)

  given [A, H <: Int, H2 <: Int, W <: Int](using H =:= H2): Conversion[Matrix[H, W, A], Matrix[H2, W, A]] =
    mH => summon[H =:= H2].liftCo[[h] =>> Matrix[h & Int, W, A]](mH)

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
                skipped: Matrix[H - 1, W, A]
            ).addTop(topVector).asH[H]
        }
        moving[1](tailMatrix, Matrix { Vector.of(topVector) })
      }
  }

  def onZeroColumn[H <: Int, W <: Int, A: Div: Mul: Sub: Zero: One](
    height: H,
    maybeRightMatrixToProcess: Either[W =:= 1, Matrix[H, W - 1, A]],
  )(using Evidence[H > 0]): SubMatrixResult[H, W, A] =
    val zero                     = Zero.of[A]
    val zeroColumn: Vector[H, A] = Vector.fill(height)(zero)
    maybeRightMatrixToProcess match
      case Right(rightMatrixToProcess) =>
        val SubMatrixResult(rightMatrix, toBeSubtractedAbove) = recursive(rightMatrixToProcess)
        SubMatrixResult(rightMatrix.addLeft(zeroColumn).asW[W], toBeSubtractedAbove)
      case Left(is1) =>
        val wIs1: Vector[1, A] =:= Vector[W, A] = is1.liftContra[[s] =>> Vector[s & Int, A]]
        SubMatrixResult(
          Matrix(zeroColumn.map(zero => wIs1(Vector.of[A](zero)))),
          StdVector.empty,
        )

  def desplitTop[H <: Int, W <: Int, A](
    top: Vector[W, A],
    tail: Either[H =:= 1, Matrix[H - 1, W, A]],
  ): Matrix[H, W, A] =
    tail match
      case Right(tailMatrix) => tailMatrix.addTop(top).asH[H]
      case Left(is1) =>
        given =:=[H, 1] = is1
        Matrix(Vector.of(top))

  def desplit[Size <: Int, A](lead: A, tail: Either[Size =:= 1, Vector[Size - 1, A]]): Vector[Size, A] =
    tail match
      case Right(tailVector) => (lead +: tailVector).asS[Size]
      case Left(is1) =>
        given =:=[Size, 1] = is1
        Vector.of(lead)

  def dropZeroColumn[H <: Int, W <: Int, A](
    topVectorTail: Vector[W - 1, A],
    maybeTailMatrix: Either[H =:= 1, Matrix[H - 1, W, A]],
  ): Matrix[H, W - 1, A] =
    import topVectorTail.sizeEvidence
    desplitTop(
      topVectorTail,
      maybeTailMatrix.map(_.leftTail),
    )

  def subtractBy[A: Mul: Sub](coefficient: A)(x: A, base: A): A = x - base * coefficient

  def subtractDown[H <: Int, W <: Int, A: Div: Mul: Sub: Zero](
    topVectorLead: A,
    maybeTopVectorTail: Either[W =:= 1, Vector[W - 1, A]],
    maybeTailMatrix: Either[H =:= 1, Matrix[H - 1, W, A]],
  ): Either[H =:= 1 | W =:= 1 | (H =:= 1, W =:= 1), Matrix[H - 1, W - 1, A]] =
    maybeTopVectorTail match
      case Right(topVectorTail) =>
        maybeTailMatrix.map { tailMatrix =>
          import topVectorTail.sizeEvidence
          tailMatrix.mapRows { tailMatrixRow =>
            val rowLead                   = tailMatrixRow.head
            val rowTail: Vector[W - 1, A] = tailMatrixRow.tail
            if rowLead == Zero.of[A] then rowTail
            else Vector.map2(rowTail, topVectorTail)(subtractBy(rowLead / topVectorLead))
          }
        }
      case Left(wIs1) =>
        Left {
          maybeTailMatrix match
            case Left(hIs1) => hIs1 -> wIs1
            case _          => wIs1
        }

  def sliceByPattern[Size <: Int, PatternSize <: Int, A](
    vector: Vector[Size, A],
    pattern: Option[Vector[PatternSize, A]],
  ): (Option[Vector[Size - PatternSize - 1, A]], A, Option[Vector[PatternSize, A]]) = ???
  // pattern match
  //   case None =>
  //     (vector.init.asInstanceOf[Option[Vector[Size - PatternSize - 1, A]]], vector.last, None)
  //   case Some(patternVector) =>
  //     val subtractionPart = vector.slice[Size - PatternSize, Size](vector.size - patternVector.size, vector.size)
  //     val localLead       = vector(vector.size - patternVector.size - 1)
  //     val maybeRest: Vector[Size - PatternSize - 1, A] = Option.when(vector.size - pattern.size - 1 > 0) {
  //       vector.slice[0, Size - PatternSize - 1](0, vector.size - pattern.size - 1)
  //     }
  //     (maybeRest, localLead, Some(subtractionPart))

  def subtractBack[A: Mul: Sub](
    toSubtract: StdVector[NonEmptyList[Option[Vector[_, A]]]],
    vector: Vector[_, A],
  ): NonEmptyList[Option[Vector[_, A]]] =
    val (maybeRest, subtractedPartsList) = toSubtract.foldLeft((Some(vector), List.empty[Option[Vector[_, A]]])) {
      case ((Some(vector), parts), currentToSubtract) =>
        val splitPattern                         = currentToSubtract.head
        val (maybeRest, localLead, maybeNewPart) = sliceByPattern(vector, ??? /* splitPattern */ )

        val subtractedParts = map2(maybeNewPart +: parts, currentToSubtract) { (maybeVectorPart, maybeToSubtractPart) =>
          map2(maybeVectorPart, maybeToSubtractPart) { (vectorPart, toSubtractPart) =>
            Vector.map2(vectorPart, ??? /* toSubtractPart */ )(subtractBy(localLead))
          }
        }
        (??? /* maybeRest */, subtractedParts)
      case (acc, _) => acc
    }
    maybeRest :: subtractedPartsList

  def divideByLead[W <: Int, E, A: Div](lead: A, maybeVector: Either[E, Vector[W, A]]): Either[E, Vector[W, A]] =
    maybeVector.map(_.map(_ / lead))

  def composeVectorParts[Size <: Int, A: Zero](
    parts: NonEmptyList[Option[Vector[_, A]]]
  ): Either[Size =:= 1, Vector[Size - 1, A]] =
    ??? // parts.map(desplit(Zero.of[A], _)).reduce((l, r) => l ++ r).tail

  def onDownSubtraction[H <: Int, W <: Int, A: Div: Mul: Sub: Zero: One](
    topLead: A,
    maybeTopTail: Either[W =:= 1, Vector[W - 1, A]],
    maybeDownRightMatrixToProcess: Either[H =:= 1 | W =:= 1 | (H =:= 1, W =:= 1), Matrix[H - 1, W - 1, A]],
  ): SubMatrixResult[H, W, A] =
    maybeDownRightMatrixToProcess match
      case Left(evidence) =>
        evidence match
          case hIs1: =:=[H, 1] =>
            val maybeDividedTail = divideByLead(topLead, maybeTopTail)
            SubMatrixResult(
              desplitTop(desplit(One.of[A], maybeDividedTail), Left(hIs1)),
              StdVector(NonEmptyList(maybeDividedTail.toOption)),
            )
          case wIs1: =:=[W, 1] =>
            ???

          case (hIs1, wIs1) =>
            SubMatrixResult(
              desplitTop(desplit(One.of[A], ???), Left(hIs1)),
              ???,
            )

      // val maybeDividedTail = divideByLead(topLead, maybeTopTail)
      // SubMatrixResult(
      //   desplitTop(desplit(One.of[A], maybeDividedTail), Left(hIs1)),
      //   StdVector(NonEmptyList(maybeDividedTail.toOption)),
      // )
      case Right(downRightMatrixToProcess) =>
        val SubMatrixResult(downRightMatrix, toSubtract) = recursive(downRightMatrixToProcess)

        val maybeSubtractedTailParts: Option[NonEmptyList[Option[Vector[_, A]]]] =
          maybeTopTail.map(subtractBack(toSubtract, _)).toOption

        val maybeDividedSubtractedTailParts: Option[NonEmptyList[Option[Vector[_, A]]]] =
          ??? // maybeSubtractedTailParts.map(_.map(divideByLead(topLead, _)))

        val maybeComposedVectorTail: Either[W =:= 1, Vector[W - 1, A]] =
          ??? // maybeDividedSubtractedTailParts.flatMap(composeVectorParts)

        val topVector: Vector[W, A] = desplit(One.of[A], maybeComposedVectorTail)
        val zeroedDownMatrix        = downRightMatrix.mapRows(Zero.of[A] +: _).asW[W]
        SubMatrixResult(
          zeroedDownMatrix.addTop[A](topVector).asH[H],
          maybeDividedSubtractedTailParts.fold(toSubtract)(toSubtract :+ _),
        )

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
        maybeTopVectorTail,
        subtractDown(topVectorLead, maybeTopVectorTail, maybeMatrixTail),
      )
  }

@main def test = {
  val matrix: Matrix[4, 2, Double] = Matrix {
    Vector.of[4, Vector[2, Double]](
      Vector.of[2, Double](0.0, 1.0),
      Vector.of[2, Double](0.0, 2.0),
      Vector.of[2, Double](1.0, 2.0),
      Vector.of[2, Double](0.0, 3.0),
    )
  }

  val matrix2: Matrix[4, 2, Double] = Matrix {
    Vector.of[4, Vector[2, Double]](
      Vector.of[2, Double](0.0, 0.0),
      Vector.of[2, Double](0.0, 0.0),
      Vector.of[2, Double](0.0, 0.0),
      Vector.of[2, Double](0.0, 0.0),
    )
  }
  println(GaussianElimination.recursive(matrix2))

}