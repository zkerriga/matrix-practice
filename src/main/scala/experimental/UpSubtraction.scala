package experimental

import matrix.Vector
import scala.compiletime.ops.int.*

object UpSubtraction {

  enum Structure[H <: Int, W <: Int, A]:
    case Empty extends Structure[0, 0, Nothing]

    case Zero[H <: Int, W <: Int]() extends Structure[H, W, Nothing]

    case Node1[W <: Int, A](
      line1: VectorNode[W, A]
    ) extends Structure[1, W, A]

    case Node2[W <: Int, A](
      line1: VectorNode[W, A],
      line2: VectorNode[W - 1, A],
    ) extends Structure[2, W, A]

    case Node3[W <: Int, A](
      line1: VectorNode[W, A],
      line2: VectorNode[W - 1, A],
      line3: VectorNode[W - 2, A],
    ) extends Structure[3, W, A]

  extension [R](a: R) def asRight[L]: Either[L, R] = Right(a)

  // case class VecNode[W <: Int,  A]()
  enum VectorNode[Size <: Int, A]:
    case Lead[Size <: Int, A](lead: A, tail: Either[Size =:= 1, VectorNode[Size - 1, A]]) extends VectorNode[Size, A]
    case Tail[Size <: Int, A](vector: Vector[Size, A])                                    extends VectorNode[Size, A]

  val example1: VectorNode[4, Int] = {
    import VectorNode.*
    Lead[4, Int](1, Lead[3, Int](2, Tail[2, Int](Vector.of(3, 4)).asRight).asRight)
  }
  // Empty

  // 10 [11 12 13 14 15 16 17 18 19] <-- S - 1
  // Empty

  // 10 (11 [12 13 14 15 16 17 18 19] <-- S - 2)
  //     20 [21 22 23 24 25 26 27 28] <-- S - 2
  // Empty

  // 10 (11 (12 [13 14 15 16 17 18 19] <-- S - 3))
  //     20 (21 [22 23 24 25 26 27 28] <-- S - 3)
  //         ZZ
  // Empty

  // 10 (11 (12 (13 [14 15 16 17 18 19] <-- S - 4)))
  //     20 (21 (22 [23 24 25 26 27 28] <-- S - 4))
  //         ZZ
  //             31 [32 33 34 35 36 37] <-- S - 4
  // Empty
}
