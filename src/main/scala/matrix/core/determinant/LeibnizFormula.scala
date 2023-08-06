package matrix.core.determinant

import math.aliases.*
import math.syntax.*
import matrix.Matrix

/**
 * @see
 *   [[https://en.wikipedia.org/wiki/Leibniz_formula_for_determinants]]
 */
object LeibnizFormula:
  def determinant2x2[A: Mul: Sub](matrix: Matrix[2, 2, A]): A =
    matrix(0, 0) * matrix(1, 1) - matrix(0, 1) * matrix(1, 0)

  val `2x2`: DeterminantAlgorithm[2] = new:
    def det[A: Mul: Sub: Add](matrix: Matrix[2, 2, A]): A = determinant2x2(matrix)
