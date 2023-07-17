package experimental

import scala.annotation.tailrec

type A           = Float
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
  if headVector.head == 0.0f then
    maybeTailVectors match
      case Some(tailVectors) => moveNonZeroLeadRowToTheTopInternal(tailVectors, headVector :: skippedRows)
      case None              => headVector :: skippedRows
  else headVector :: maybeTailVectors.fold(skippedRows) { tailVectors => skippedRows ++ tailVectors }

def moveNonZeroLeadRowToTheTop(matrix: MyVector[MyVector[A]]): MyVector[MyVector[A]] =
  val (headVector, maybeTailVectors) = split(matrix)
  if headVector.head != 0.0f then matrix
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

  if headVectorLead == 0.0f then
    RecursionDownResult.ZeroColumnLeft(
      matrix.length,
      combineOptions(maybeHeadVectorTail, maybeTailVectors) { (headVectorTail, tailVectors) =>
        startFunction(headVectorTail :: tailVectors) // todo: smaller matrix -> start again
      },
    )
  else
    // todo: size here is less by 1
    val processSubtractDivision: Option[MyVector[MyVector[A]]] = maybeTailVectors.flatMap { tailVectors =>
      tailVectors.traverse { tailVectorsRow =>
        val (rowLead, maybeRowTail) = split(tailVectorsRow)
        if rowLead == 0.0f then maybeRowTail
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
        case Some(rightMatrixResult) => composeResult(rightMatrixResult).map(0.0f :: _)
        case None                    => List.fill(height)(List(0.0f))

    case RecursionDownResult.DownSubtractDivision(headLead, maybeHeadTail, maybeMatrixOnDownRight) =>
      val headVector = headLead :: maybeHeadTail.toList.flatten
      maybeMatrixOnDownRight match
        case Some(downRightMatrixResult) =>
          val downRightMatrix = composeResult(downRightMatrixResult)
          headVector :: downRightMatrix.map(0.0f :: _)
        case None => List(headVector)
}

def printMatrix(matrix: List[List[Float]]): Unit = {
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

  val result   = startFunction(matrix2)
  val composed = composeResult(result)
  printMatrix(composed)
}
