package experimental

import java.math.{MathContext, RoundingMode}
import scala.annotation.tailrec

type A = BigDecimal
val Zero: A = BigDecimal(0)
val One: A  = BigDecimal(1)

type MyVector[B] = List[B]

import scala.annotation.tailrec

extension [A](list: List[A]) {
  def traverse[B](f: A => Option[B]): Option[List[B]] = {
    @tailrec
    def traverseHelper(remaining: List[A], acc: List[B]): Option[List[B]] = {
      remaining match {
        case Nil => Some(acc.reverse)
        case head :: tail =>
          f(head) match {
            case Some(b) => traverseHelper(tail, b :: acc)
            case None    => None
          }
      }
    }

    traverseHelper(list, Nil)
  }
}

def combineOptions[A, B, C](optionA: Option[A], optionB: Option[B])(func: (A, B) => C): Option[C] =
  optionA.flatMap(a => optionB.map(b => func(a, b)))

def map2(a: MyVector[A], b: MyVector[A])(f: (A, A) => A): MyVector[A] =
  a.zip(b).map { case (aX, bX) => f(aX, bX) }

def lmap2[A](a: List[A], b: List[A])(f: (A, A) => A): List[A] =
  a.zip(b).map { case (aX, bX) => f(aX, bX) }

def desplit[A](lead: A, tail: Option[MyVector[A]]): MyVector[A] =
  lead :: tail.getOrElse(Nil)

def split[A](vector: MyVector[A]): (A, Option[MyVector[A]]) =
  vector match
    case element :: Nil        => element -> None
    case element :: tailVector => element -> Some(tailVector)

@tailrec
def moveNonZeroLeadRowToTheTopInternal(
  matrixLeft: MyVector[MyVector[A]],
  skippedRows: MyVector[MyVector[A]],
): MyVector[MyVector[A]] =
  val (headVector, maybeTailVectors) = split(matrixLeft)
  if headVector.head == Zero then
    maybeTailVectors match
      case Some(tailVectors) => moveNonZeroLeadRowToTheTopInternal(tailVectors, headVector :: skippedRows)
      case None              => headVector :: skippedRows
  else headVector :: maybeTailVectors.fold(skippedRows) { tailVectors => skippedRows ++ tailVectors }

def moveNonZeroLeadRowToTheTop(matrix: MyVector[MyVector[A]]): MyVector[MyVector[A]] =
  val (headVector, maybeTailVectors) = split(matrix)
  if headVector.head != Zero then matrix
  else
    maybeTailVectors.fold(matrix) { tailVectors =>
      moveNonZeroLeadRowToTheTopInternal(tailVectors, List(headVector))
    }

enum RecursionDownResult:
  case ZeroColumnLeft(height: Int, maybeMatrixOnRight: Option[RecursionDownResult])
  case DownSubtractDivision(
    headLead: A,
    maybeHeadTail: Option[MyVector[A]],
    maybeMatrixOnDownRight: Option[RecursionDownResult],
  )

def startFunction(matrix: MyVector[MyVector[A]]): RecursionDownResult = {
  val swappedMatrix = moveNonZeroLeadRowToTheTop(matrix)

  val (headVector, maybeTailVectors)        = split(swappedMatrix)
  val (headVectorLead, maybeHeadVectorTail) = split(headVector)

  if headVectorLead == Zero then
    RecursionDownResult.ZeroColumnLeft(
      matrix.length,
      maybeHeadVectorTail.map { headVectorTail =>
        // todo: right matrix -> go again
        startFunction(maybeTailVectors.fold(List(headVectorTail)) { tailVectors =>
          headVectorTail :: tailVectors.map(_.tail)
        })
      },
    )
  else
    // todo: size here is less by 1
    val processSubtractDivision: Option[MyVector[MyVector[A]]] = maybeTailVectors.flatMap { tailVectors =>
      tailVectors.traverse { tailVectorsRow =>
        val (rowLead, maybeRowTail) = split(tailVectorsRow)
        if rowLead == Zero then maybeRowTail
        else
          val coefficient = rowLead / headVectorLead
          combineOptions(maybeHeadVectorTail, maybeRowTail) { (headVectorTail, rowTail) =>
            map2(headVectorTail, rowTail)((baseX, rowX) => rowX - baseX * coefficient)
          }
      }
    }
    RecursionDownResult.DownSubtractDivision(
      headLead = headVectorLead,
      maybeHeadTail = maybeHeadVectorTail,
      maybeMatrixOnDownRight = processSubtractDivision.map(smallerMatrix => startFunction(smallerMatrix)),
    )
}

def composeResult(result: RecursionDownResult): List[List[A]] = {
  result match
    case RecursionDownResult.ZeroColumnLeft(height, maybeMatrixOnRight) =>
      maybeMatrixOnRight match
        case Some(rightMatrixResult) => composeResult(rightMatrixResult).map(Zero :: _)
        case None                    => List.fill(height)(List(Zero))

    case RecursionDownResult.DownSubtractDivision(headLead, maybeHeadTail, maybeMatrixOnDownRight) =>
      val headVector = headLead :: maybeHeadTail.toList.flatten
      maybeMatrixOnDownRight match
        case Some(downRightMatrixResult) =>
          val downRightMatrix = composeResult(downRightMatrixResult)
          headVector :: downRightMatrix.map(Zero :: _)
        case None => List(headVector)
}

case class Ready(downRightMatrix: MyVector[MyVector[A]], toBeSubtractedAbove: List[List[Option[MyVector[A]]]])

def sliceRightForSubtract(
  vector: MyVector[A],
  partToSubtract: Option[MyVector[A]],
): (Option[MyVector[A]], A, Option[MyVector[A]]) =
  partToSubtract match
    case None =>
      val localLead = vector.takeRight(1).head
      val maybeRest = vector.dropRight(1)
      (Option.when(maybeRest.nonEmpty)(maybeRest), localLead, None)

    case Some(vectorToSubtract) =>
      val localLead :: subtractionPart = vector.takeRight(vectorToSubtract.length + 1)
      val maybeRest                    = vector.dropRight(vectorToSubtract.length + 1)
      (Option.when(maybeRest.nonEmpty)(maybeRest), localLead, Some(subtractionPart))

@tailrec
def backSubtraction(
  toBeSubtracted: List[List[Option[MyVector[A]]]],
  mainToSplit: MyVector[A],
  vectorParts: List[Option[MyVector[A]]] = List.empty,
): List[Option[MyVector[A]]] =
  toBeSubtracted match
    case Nil => Some(mainToSplit) :: vectorParts
    case currentToSubtract :: othersToBeSubtracted =>
      val splitPattern                                   = currentToSubtract.head
      val (maybeRest, localLead, maybeNewPartToSubtract) = sliceRightForSubtract(mainToSplit, splitPattern)

      val allVectorPartsToBeSubtracted = maybeNewPartToSubtract :: vectorParts
      val subtractedParts = lmap2[Option[MyVector[A]]](allVectorPartsToBeSubtracted, currentToSubtract) {
        (maybeHeadVector, maybeToSubtractVector) =>
          combineOptions(maybeHeadVector, maybeToSubtractVector) { (headVector, toSubtractVector) =>
            map2(headVector, toSubtractVector) { (headX, toSubtractX) =>
              headX - toSubtractX * localLead
            }
          }
      }
      maybeRest match
        case Some(restVector) =>
          backSubtraction(
            othersToBeSubtracted,
            restVector,
            subtractedParts,
          )
        case None =>
          maybeRest :: subtractedParts

def upDecomposition(result: RecursionDownResult): Ready = {
  result match
    case RecursionDownResult.ZeroColumnLeft(height, maybeMatrixOnRightResult) =>
      maybeMatrixOnRightResult match
        case Some(maybeMatrixOnRightResult) =>
          val Ready(rightMatrix, toBeSubtractedAbove) = upDecomposition(maybeMatrixOnRightResult)
          Ready(rightMatrix.map(Zero :: _), toBeSubtractedAbove)
        case None => Ready(List.fill(height)(List(Zero)), List.empty)

    case RecursionDownResult.DownSubtractDivision(headLead, maybeHeadTail, maybeMatrixOnDownRight) =>
      maybeMatrixOnDownRight match
        case Some(downRightMatrixResult) =>
          val Ready(downRightMatrix, toBeSubtracted) = upDecomposition(downRightMatrixResult)

          val subtractedTailParts: List[Option[MyVector[A]]] = maybeHeadTail
            .map { headVectorTail =>
              backSubtraction(toBeSubtracted, headVectorTail)
            }.getOrElse(Nil)

          val maybeDividedSubtractedTailPats: List[Option[MyVector[A]]] =
            subtractedTailParts.map(_.map(_.map(_ / headLead)))

          val composedDividedSubtractedTail: Option[MyVector[A]] = {
            val list = maybeDividedSubtractedTailPats
              .flatMap { maybePart =>
                Zero :: maybePart.getOrElse(Nil)
              }.drop(1)
            Option.when(list.nonEmpty)(list)
          }
          val headVector       = desplit[A](One, composedDividedSubtractedTail)
          val zeroedDownMatrix = downRightMatrix.map(Zero :: _)
          Ready(
            headVector :: zeroedDownMatrix,
            if maybeDividedSubtractedTailPats.nonEmpty then toBeSubtracted :+ maybeDividedSubtractedTailPats
            else toBeSubtracted,
          )
        case None =>
          val maybeDividedTail = maybeHeadTail.map(_.map(_ / headLead))
          val downRight        = List(desplit[A](One, maybeDividedTail))
          Ready(downRight, List(List(maybeDividedTail)))
}

def printMatrix(matrix: List[List[A]]): Unit = {
  // Get the number of rows and columns in the matrix
  val numRows = matrix.length
  val numCols = if (numRows > 0) matrix.head.length else 0

  // Find the maximum length of the elements in the matrix
  val maxElementLength = matrix.flatten.map(_.toString.length).max

  // Print the matrix
  for (row <- matrix) {
    for (element <- row) {
      val paddedElement = element.toString.padTo(maxElementLength, ' ')
      print(s"$paddedElement ")
    }
    println()
  }
}

@main def test = {
  println("start!")

  given Conversion[Int, A] = int => BigDecimal(int, new MathContext(10, RoundingMode.FLOOR))

  val matrix1: MyVector[MyVector[A]] = List(
    List(0, 2, 3, 4),
    List(0, 6, 7, 8),
    List(0, 3, 2, 1),
  )

  val matrix2: MyVector[MyVector[A]] = List(
    List(1, 3, 1, 9),
    List(1, 3, 5, 9),
    List(1, 1, -1, 1),
  )

  val matrix3: MyVector[MyVector[A]] = List(
    List(1, 2, 3, 4, 5, 6),
    List(0, 0, 1, 2, 3, 4),
    List(0, 0, 0, 0, 1, 2),
  )

  val matrix4: MyVector[MyVector[A]] = List(
    List(1, 2, 3, 4, 5, 6),
    List(0, 0, 1, 2, 3, 4),
    List(0, 0, 0, 0, 0, 0),
  )

  val matrix5: MyVector[MyVector[A]] = List(
    List(0, 0, 0, 0),
    List(0, 0, 0, 0),
    List(0, 0, 0, 0),
  )

  val matrix6: MyVector[MyVector[A]] = List(
    List(1, 2, 3, 4, 5, 6, 7, 8, 9),
    List(0, 0, 0, 1, 2, 3, 4, 5, 6),
    List(0, 0, 0, 0, 0, 0, 1, 2, 3),
  )

  def process(matrix: MyVector[MyVector[A]]): Unit = {
    val result = startFunction(matrix)
    println(result)
    val composed = composeResult(result)
    printMatrix(composed)

    val decomposed = upDecomposition(result)
    println(decomposed)
    printMatrix(decomposed.downRightMatrix)
  }

  process(matrix1)
  process(matrix2)
  process(matrix3)
  process(matrix4)
  process(matrix5)
  process(matrix6)

  val randomMatrix1: MyVector[MyVector[A]] = List(
    List(4, 19, 0, 1),
    List(42, 4, -5, 0),
    List(0, 1, -9, 20),
  )
  // the answer is Identity + [-0.2736, 0.1102, -2.20997]
  process(randomMatrix1)

  val randomMatrix2: MyVector[MyVector[A]] = List(
    List(-1, -2, -3, -4, -5, -6),
    List(-1, -2, 1, 2, 3, 4),
    List(5, 6, 7, 8, 9, 10),
    List(-5, -6, -7, -8, -9, -10),
  )
  /* the answer is:
	1	0	0	-0.5	-1	-1.5
	0	1	0	0	0	0
	0	0	1	1.5	2	2.5
	0	0	0	0	0	0
   */
  process(randomMatrix2)

  val randomMatrix3: MyVector[MyVector[A]] = List(
    List(2, 4, -2, 6),
    List(1, 2, -1, 3),
    List(3, 2, 1, 11),
  )
  /* the answer is:
	[ 1  0  1 |  4 ]
    [ 0  1 -1 |  -0.5 ]
    [ 0  0  0 |  0 ]
   */
  process(randomMatrix3)

  val randomMatrix4: MyVector[MyVector[A]] = List( // todo: figure out why failed
    List(2, 1, 3, 4, 5, 6, 7, 8, 9, 10, 11),
    List(12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22),
    List(23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33),
    List(34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44),
    List(45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55),
    List(56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66),
    List(67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77),
    List(78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88),
    List(89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99),
    List(100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110),
  )
  /* the answer is:
    X1	X2	X3	X4	X5	X6	X7	X8	X9	X10	b
    1	0	0	-1/3	-2/3	-1	-4/3	-5/3	-2	-7/3	-8/3
    0	1	0	-1/3	-2/3	-1	-4/3	-5/3	-2	-7/3	-8/3
    0	0	1	5/3	7/3	3	11/3	13/3	5	17/3	19/3
    0	0	0	0	0	0	0	0	0	0	0
    0	0	0	0	0	0	0	0	0	0	0
    0	0	0	0	0	0	0	0	0	0	0
    0	0	0	0	0	0	0	0	0	0	0
    0	0	0	0	0	0	0	0	0	0	0
    0	0	0	0	0	0	0	0	0	0	0
    0	0	0	0	0	0	0	0	0	0	0
   */
  process(randomMatrix4)
}
