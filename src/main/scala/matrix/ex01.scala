package matrix

import utils.{HMul, Semigroup}

def linearCombination[N <: Int, Size <: Int, A: HMul.Homo: Semigroup](
  vectors: Vector[N, Vector[Size, A]],
  coefficients: Vector[N, A],
): Vector[Size, A] =
  Vector
    .map2(vectors, coefficients)(_ * _)
    .reduceLeft(_ + _)
