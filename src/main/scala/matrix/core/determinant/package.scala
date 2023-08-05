package matrix.core

import math.aliases.*
import matrix.Matrix

package object determinant {
  def determinant1x1[A](matrix: Matrix[1, 1, A]): A = matrix(0, 0)

  type DeterminantAlgorithm[Size <: Int, A] = (Mul[A], Sub[A], Add[A]) ?=> Matrix[Size, Size, A] => A
}
