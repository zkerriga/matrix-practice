package matrix.core

import math.aliases.*
import matrix.Matrix

package object determinant:
  def determinant1x1[A](matrix: Matrix[1, 1, A]): A = matrix(0, 0)

  trait DeterminantAlgorithm[Size <: Int]:
    def det[A: Mul: Sub: Add](matrix: Matrix[Size, Size, A]): A
