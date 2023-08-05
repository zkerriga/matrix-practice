package matrix.core.determinant

import math.aliases.*
import math.syntax.*
import matrix.Matrix

object LeibnizFormula:
  def determinant2x2[A: Mul: Sub](matrix: Matrix[2, 2, A]): A =
    matrix(0, 0) * matrix(1, 1) - matrix(0, 1) * matrix(1, 0)
