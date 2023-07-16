package experimental

import scala.annotation.tailrec

type A           = Float
type MyVector[B] = List[B]

@tailrec
def antizeros(
  vector: MyVector[A],
  zeros: List[0.0f] = List.empty,
  nonZeros: List[A] = List.empty,
): (List[0.0f], List[A]) =
  vector match
    case lead :: Nil =>
      if lead == 0 then (0 :: zeros, nonZeros.reverse)
      else (zeros, (lead :: nonZeros).reverse)
    case lead :: tail =>
      if lead == 0 then antizeros(tail, 0 :: zeros, nonZeros)
      else antizeros(tail, zeros, lead :: nonZeros)

enum ExtractZeroLeadsResult:
  case Vec(vector: MyVector[A])
  case ZeroLead(zeros: MyVector[0.0f], vector: MyVector[A])
  case Zeros(zeros: MyVector[0.0f])

def extractZeroLeads(vector: MyVector[A]): ExtractZeroLeadsResult =
  antizeros(vector) match
    case (Nil, nonZeros)   => ExtractZeroLeadsResult.Vec(nonZeros)
    case (zeros, Nil)      => ExtractZeroLeadsResult.Zeros(zeros)
    case (zeros, nonZeros) => ExtractZeroLeadsResult.ZeroLead(zeros, nonZeros)

//

enum Result:
  case Zeros(zeros: MyVector[0.0f])
  case ZerosAndDivided(zeros: MyVector[0.0f], divisionResul: DivisionResul)
  case Divided(divisionResul: DivisionResul)

enum DivisionResul:
  case OnlyLead(lead: 1.0f)
  case Leaded(lead: 1.0f, vector: MyVector[A])

def divide(vector: MyVector[A]): DivisionResul =
  vector match
    case _ :: Nil     => DivisionResul.OnlyLead(1.0f)
    case lead :: tail => DivisionResul.Leaded(1.0f, tail.map(_ / lead))

//

/*
def reduced(matrix: list[list], layer=0):
        # matrix has different vectors
        head_vector = matrix[0]
        tail_vectors = matrix[1:]

        head_vector_lead = head_vector[0]
        head_vector_tail = head_vector[1:]

        def zeroing_tail_vector(tail_vector: list):
            tail_vector_lead = tail_vector[0]
            tail_vector_tail = tail_vector[1:]

            if tail_vector_lead == 0:
                return tail_vector_tail

            coefficient = tail_vector_lead / head_vector_lead

            processed_tail_vector = map2(head_vector_tail, tail_vector_tail, lambda head_vector_tail_x, tail_vector_tail_x: tail_vector_tail_x - head_vector_tail_x * coefficient)
            return processed_tail_vector

        zeroed_tail_vectors = list(map(zeroing_tail_vector, tail_vectors))
        sorted_zeroed_tail_vectors = sorted(zeroed_tail_vectors, key=count_lead_zeroes)

        processed_tail_vectors = reduced(w, layer + 1) # recursive call

        result = [lead_to_be_one(head_vector)] + list(map(lambda vector: [0] + vector, processed_tail_vectors))
 */

//def findFirstNonZeroInternal(searchingMatrix: List[MyVector[A]], swappedMatrix: List[MyVector[A]] = List.empty): (MyVector[A], Unit) =
//  searchingMatrix match
//    case headVector :: tail =>
//      val lead :: tail = headVector
//      if lead != 0.0f then (headVector, ())
//    case Nil => ???

enum MatrixState: // on matrix with Height >= 2
  case Zeros(zerosInEachRow: MyVector[0.0f])
  case ZeroLead(zerosInEachRow: MyVector[0.0f], matrix: MyVector[MyVector[A]])
  case Matrix(matrix: MyVector[MyVector[A]])

@tailrec
def findFirstNonZeroInternal(
  matrixIterator: List[MyVector[A]],
  zerosInEachRow: List[0.0f] = List.empty,
  minusOneSizeMatrix: List[MyVector[A]] = List.empty,
): (List[0.0f], List[MyVector[A]]) =
  matrixIterator match
    case Nil =>
      if minusOneSizeMatrix.nonEmpty then
        findFirstNonZeroInternal(minusOneSizeMatrix, 0.0f :: zerosInEachRow, List.empty)
      else (zerosInEachRow, List.empty)
    case headVector :: tailMatrix =>
      /*
       * [
       *   headVector [0]
       *   tailMatrix [y]
       *   tailMatrix [y]
       *   tailMatrix [y]
       * ]
       * */
      headVector match
        case lead :: Nil =>
          if lead == 0 then ???
          else ???
        case lead :: tailVector => ???

// on matrix with Height >= 2
def findFirstNonZero(matrix: MyVector[MyVector[A]]): MatrixState =
  matrix match
    case headVector :: Nil =>
      headVector match
        case lead :: Nil =>
          if lead == 0 then MatrixState.Zeros(headVector)
          else MatrixState.Matrix(matrix)
        case lead :: tailVector =>
          if lead == 0 then ???
          else MatrixState.Matrix(matrix)

    case headVector :: tailVectors => ???

def reduceLogic(matrix: MyVector[MyVector[A]]) =
  matrix match
    case vector :: Nil =>
      extractZeroLeads(vector) match
        case ExtractZeroLeadsResult.Vec(vector)             => Result.Divided(divide(vector))
        case ExtractZeroLeadsResult.ZeroLead(zeros, vector) => Result.ZerosAndDivided(zeros, divide(vector))
        case ExtractZeroLeadsResult.Zeros(zeros)            => Result.Zeros(zeros)

    case matrix => // Height >= 2
      ???

@main def test = {

  val result14 = reduceLogic(List(List(0, 0, 2, 4)))
  println(result14)
}
