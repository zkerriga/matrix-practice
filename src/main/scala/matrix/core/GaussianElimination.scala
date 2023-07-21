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

  def moveNonZeroLeadRowToTop[H <: Int, W <: Int, A: Zero](matrix: Matrix[H, W, A]): Matrix[H, W, A] = {
    val topVector = matrix.topRow
    if topVector.head != Zero.of[A] then matrix
    else
      matrix.topTail.fold(matrix) { tailMatrix =>
        @tailrec
        def moving[I <: Int](left: Matrix[H - I, W, A], skipped: Matrix[I, W, A]): Matrix[H, W, A] = {
          val topVector       = left.topRow
          val maybeTailMatrix = left.topTail
          if topVector.head == Zero.of[A] then
            maybeTailMatrix match
              case Some(tailMatrix) => moving(tailMatrix.asH[H - (I + 1)], skipped.addDown(topVector))
              case None             => matrix
          else
            (maybeTailMatrix match
              case Some(tailMatrix) => tailMatrix.addTop(skipped).asH[H - 1]
              case None             => skipped.asInstanceOf[Matrix[H - 1, W, A]]
            ).addTop(topVector).asH[H]
        }
        moving[1](tailMatrix, Matrix { Vector.of(topVector) })
      }
  }

  def onZeroColumn[H <: Int, W <: Int, A: Div: Mul: Sub: Zero: One](
    height: H,
    maybeRightMatrixToProcess: Option[Matrix[H, W, A]],
  )(using Evidence[H > 0]): SubMatrixResult[H, W + 1, A] =
    val zero                     = Zero.of[A]
    val zeroColumn: Vector[H, A] = Vector.fill(height)(zero)
    maybeRightMatrixToProcess match
      case Some(rightMatrixToProcess) =>
        val SubMatrixResult(rightMatrix, toBeSubtractedAbove) = recursive(rightMatrixToProcess)
        SubMatrixResult(rightMatrix.addLeft(zeroColumn), toBeSubtractedAbove)
      case None =>
        SubMatrixResult(Matrix(zeroColumn.map(Vector.of[A](_).asInstanceOf[Vector[W + 1, A]])), StdVector.empty)

  def desplitTop[H <: Int, W <: Int, A](top: Vector[W, A], tail: Option[Matrix[H - 1, W, A]]): Matrix[H, W, A] =
    tail match
      case Some(tailMatrix) => tailMatrix.addTop(top).asH[H]
      case None             => Matrix(Vector.of[Vector[W, A]](top).asInstanceOf[Vector[H, Vector[W, A]]])

  def desplit[Size <: Int, A](lead: A, tail: Option[Vector[Size - 1, A]]): Vector[Size, A] =
    tail match
      case Some(tailVector) => (lead +: tailVector).asS[Size]
      case None             => Vector.of(lead).asInstanceOf[Vector[Size, A]]

  def dropZeroColumn[H <: Int, W <: Int, A](
    topVectorTail: Vector[W - 1, A],
    maybeTailMatrix: Option[Matrix[H - 1, W, A]],
  ): Matrix[H, W - 1, A] =
    import topVectorTail.sizeEvidence
    desplitTop(
      topVectorTail,
      maybeTailMatrix.map(_.leftTail),
    )

  def subtractBy[A: Mul: Sub](coefficient: A)(x: A, base: A): A = x - base * coefficient

  def subtractDown[H <: Int, W <: Int, A: Div: Mul: Sub: Zero](
    topVectorLead: A,
    maybeTopVectorTail: Option[Vector[W - 1, A]],
    maybeTailMatrix: Option[Matrix[H - 1, W, A]],
  ): Option[Matrix[H - 1, W - 1, A]] =
    map2(maybeTopVectorTail, maybeTailMatrix) { (topVectorTail, tailMatrix) =>
      import topVectorTail.sizeEvidence
      tailMatrix.mapRows { tailMatrixRow =>
        val rowLead                   = tailMatrixRow.head
        val rowTail: Vector[W - 1, A] = tailMatrixRow.tail
        if rowLead == Zero.of[A] then rowTail
        else Vector.map2(rowTail, topVectorTail)(subtractBy(rowLead / topVectorLead))
      }
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

  def divideByLead[W <: Int, A: Div](lead: A, maybeVector: Option[Vector[W, A]]): Option[Vector[W, A]] =
    maybeVector.map(_.map(_ / lead))

  def composeVectorParts[Size <: Int, A: Zero](parts: NonEmptyList[Option[Vector[_, A]]]): Option[Vector[Size, A]] =
    ??? // parts.map(desplit(Zero.of[A], _)).reduce((l, r) => l ++ r).tail

  def onDownSubtraction[H <: Int, W <: Int, A: Div: Mul: Sub: Zero: One](
    topLead: A,
    maybeTopTail: Option[Vector[W - 1, A]],
    maybeDownRightMatrixToProcess: Option[Matrix[H - 1, W - 1, A]],
  ): SubMatrixResult[H, W, A] =
    maybeDownRightMatrixToProcess match
      case None =>
        val maybeDividedTail = divideByLead(topLead, maybeTopTail)
        SubMatrixResult(
          desplitTop(desplit(One.of[A], maybeDividedTail), None),
          StdVector(NonEmptyList(maybeDividedTail)),
        )
      case Some(downRightMatrixToProcess) =>
        val SubMatrixResult(downRightMatrix, toSubtract) = recursive(downRightMatrixToProcess)

        val maybeSubtractedTailParts: Option[NonEmptyList[Option[Vector[_, A]]]] =
          maybeTopTail.map(subtractBack(toSubtract, _))

        val maybeDividedSubtractedTailParts: Option[NonEmptyList[Option[Vector[_, A]]]] =
          ??? // maybeSubtractedTailParts.map(_.map(divideByLead(topLead, _)))

        val maybeComposedVectorTail: Option[Vector[W - 1, A]] =
          maybeDividedSubtractedTailParts.flatMap(composeVectorParts)

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
    Vector.of(
      Vector.of(0.0, 1.0),
      Vector.of(0.0, 2.0),
      Vector.of(1.0, 2.0),
      Vector.of(0.0, 3.0),
    )
  }

  val matrix2: Matrix[4, 2, Double] = Matrix {
    Vector.of(
      Vector.of(0.0, 0.0),
      Vector.of(0.0, 0.0),
      Vector.of(0.0, 0.0),
      Vector.of(0.0, 0.0),
    )
  }
  println(GaussianElimination.recursive(matrix2))
}
