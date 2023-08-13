package matrix.core

import math.aliases.*
import math.{Zero, One}
import matrix.Matrix

package object inverse {
  trait InverseAlgorithm[Size <: Int]:
    def inv[A: Div: Mul: Sub: Add: Zero: One: Eq](matrix: Matrix[Size, Size, A]): Option[Matrix[Size, Size, A]]
}
