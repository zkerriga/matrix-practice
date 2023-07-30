package matrix.core

import scala.compiletime.ops.int.*

private[core] enum Trapezoid[W <: Int, A]:
  case First(down: Trapezoid.ZeroColumn | Node.Tail[W, A])
  case Next(down: Trapezoid.ZeroColumn | Node.Artificial[W, A], next: Trapezoid[W - 1, A])

private[core] object Trapezoid:
  case object ZeroColumn
  type ZeroColumn = ZeroColumn.type
