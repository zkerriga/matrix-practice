package matrix.core.echelon

import scala.compiletime.ops.int.*

/**
 * This structure is a list of [[Node]] (or [[Trapezoid.ZeroColumn]]), in which each next element has a size one more.
 * This allows us to perform the back-subtraction process in [[matrix.core.GaussianElimination]] on only the data that
 * really needs to change.
 * @example
 *   imagine that the calculation is on the following lines of the matrix and we need to subtract the second line from
 *   the first one:
 *   {{{
 *     0 0 0 0 0 1 2 3 4 5
 *     0 0 0 0 0 0 0 1 2 3
 *   }}}
 *   It is not necessary to perform the subtraction on all 20 elements. All we need to know is:
 *   - tail of the first vector -> `[2 3 4 5]`
 *   - presence of a zero column below -> [[Trapezoid.ZeroColumn]]
 *   - tail of the second vector -> [2 3]
 */
private[core] enum Trapezoid[W <: Int, A]:
  case First(down: Trapezoid.ZeroColumn | Node.Tail[W, A])
  case Next(down: Trapezoid.ZeroColumn | Node.Processed[W, A], next: Trapezoid[W - 1, A])

private[core] object Trapezoid:
  case object ZeroColumn
  type ZeroColumn = ZeroColumn.type
