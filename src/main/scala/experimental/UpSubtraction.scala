package experimental

import matrix.{Vector, Matrix}
import scala.compiletime.ops.int.*
import matrix.Evidence

object UpSubtraction {
  extension [R](a: R) def asRight[L]: Either[L, R] = Right(a)
  extension [L, R](either: Either[L, R])
    def leftMap[L2](f: L => L2): Either[L2, R] = either match
      case Left(value)  => Left(f(value))
      case Right(value) => value.asRight

  def emap2[E, A, B, C](aE: Either[E, A], bE: Either[E, B])(f: (A, B) => C): Either[E, C] =
    (aE, bE) match
      case (Right(a), Right(b)) => Right(f(a, b))
      case (Left(error), _)     => Left(error)
      case (_, Left(error))     => Left(error)

  // enum Trapezoid[H <: Int, W <: Int, +A]:
  //   case Empty[W <: Int]()                                         extends Trapezoid[0, W, Nothing]
  //   case Zero[H <: Int, W <: Int, A](next: Trapezoid[H - 1, W, A]) extends Trapezoid[H, W, A]
  //   case Node[H <: Int, W <: Int, A](lead: A, tail: Vector[W - H, A], next: Trapezoid[H - 1, W, A])
  //       extends Trapezoid[H, W, A]

  enum Trapezoid[BaseW <: Int, D <: Int, H <: Int, W <: Int, +A]:
    case Empty[BaseW <: Int]() extends Trapezoid[BaseW, 0, 0, BaseW + 1, Nothing]
    case Zero[BaseW <: Int, D <: Int, H <: Int, W <: Int, A](next: Trapezoid[BaseW, D, H, W, A])
        extends Trapezoid[BaseW, D + 1, H, W + 1, A]
    case Node[BaseW <: Int, D <: Int, H <: Int, W <: Int, A](
      lead: A,
      tail: Vector[W, A],
      next: Trapezoid[BaseW, D, H, W, A],
    ) extends Trapezoid[BaseW, D + 1, H + 1, W + 1, A]

  sealed trait Trap[BaseW <: Int, D <: Int, +A]:
    type dH <: Int
    type dW <: Int

  object Trap:
    case class Empty[BaseW <: Int]() extends Trap[BaseW, 0, Nothing]:
      override type dH = 0
      override type dW = 0

    case class Zero[BaseW <: Int, D <: Int, A](next: Trap[BaseW, D - 1, A]) extends Trap[BaseW, D, A]:
      override type dH = 0
      override type dW = 1

    case class Node[BaseW <: Int, D <: Int, A](lead: A, tail: Vector[BaseW - D, A], next: Trap[BaseW, D - 1, A])
        extends Trap[BaseW, D, A]
  end Trap

  // import Trap.*

  // val node0: Trapezoid[0, 0, 4, Int] = Empty()
  // val node1: Trapezoid[1, 1, 4, Int] = Node[1, 1, 4, Int](10, Vector.of(11, 12, 13), node0)
  // val node2: Trapezoid[2, 1, 4, Int] = Zero(node1)
  // val node3: Trapezoid[3, 2, 4, Int] = Node(30, Vector.of[Int](31), node2)

  type A = Int

  // def build[BaseW <: Int, D <: Int, H <: Int, W <: Int](
  //   trapezoid: Trap[BaseW, D, A],
  //   matrixAcc: Matrix[H, W, A],
  //   acc: Any,
  // ): Matrix[H + trapezoid.dH, W + trapezoid.dW, A] =
  //   trapezoid match
  //     case Empty() =>
  //       ??? // matrixAcc
  //     case z @ Zero(next) =>
  //       build[BaseW, D - 1, H + z.dH, W + z.dW](
  //         next,
  //         matrixAcc.addLeft(null.asInstanceOf[Vector[H, 0]] /* zeros */ ),
  //         ???,
  //       )
  //     case Node(lead, tail, next) =>
  //       val processedTail = tail
  //       val withZeros     = matrixAcc.addLeft(null.asInstanceOf[Vector[H, 0]] /* zeros */ )
  //       build[D - 1, BaseW, H + 1, _](next, withZeros.addTop(1 +: processedTail), ???)

  // Node[3, 4, _] -> Vector[4 - 3, _] + head -> Matrix[1, 2, _]
  // Node[2, 4, _] -> Vector[4 - 2, _] + head -> Matrix[2, 3, _]
  // Zero[1, 4, _] ->                         -> Matrix[2, 4, _]

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

  // 10 (11 (12 (13 (14 [15 16 17 18 19] <-- S - 5))))
  //     20 (21 (22 (23 [24 25 26 27 28] <-- S - 5)))
  //         ZZ
  //             31 (32 [33 34 35 36 37] <-- S - 5)
  //                 ZZ
  // Empty

  // 10 (11 (12 (13 (14 (15 [16 17 18 19] <-- S - 6)))))
  //     20 (21 (22 (23 (24 [25 26 27 28] <-- S - 6))))
  //         ZZ
  //             31 (32 (33 [34 35 36 37] <-- S - 6))
  //                 ZZ
  //                     ZZ
  // Empty

  // 10 (11 (12 (13 (14 (15 (16 [17 18 19] <-- S - 7))))))
  //     20 (21 (22 (23 (24 (25 [26 27 28] <-- S - 7)))))
  //         ZZ
  //             31 (32 (33 (34 [35 36 37] <-- S - 7)))
  //                 ZZ
  //                     ZZ
  //                         40 [41 42 43]
  // Empty

  // 10 (11 (12 (13 (14 (15 (16 (17 [18 19] <-- S - 8)))))))
  //     20 (21 (22 (23 (24 (25 (26 [27 28] <-- S - 8))))))
  //         ZZ
  //             31 (32 (33 (34 (35 [36 37] <-- S - 8))))
  //                 ZZ
  //                     ZZ
  //                         40 (41 [42 43] <-- S - 8)
  //                             50 [51 52] <-- S - 8
  // Empty

  // end of downing

  // 10 (11 (12 (13 (14 (15 (16 (17 [18 19] <-- S - 8)))))))
  //     20 (21 (22 (23 (24 (25 (26 [27 28] <-- S - 8))))))
  //         ZZ
  //             31 (32 (33 (34 (35 [36 37] <-- S - 8))))
  //                 ZZ
  //                     ZZ
  //                         40 (41 [42 43] <-- S - 8)
  //                             __ [51* 52*] <-- S - 8
  // Empty

  // 10 (11 (12 (13 (14 (15 (16 (17 [18 19] <-- S - 8)))))))
  //     20 (21 (22 (23 (24 (25 (26 [27 28] <-- S - 8))))))
  //         ZZ
  //             31 (32 (33 (34 (35 [36 37] <-- S - 8))))
  //                 ZZ
  //                     ZZ
  //                         40 (00 [42* 43*] <-- S - 8)
  //                             __ [51 52] <-- S - 8
  // Empty

  // 10 (11 (12 (13 (14 (15 (16 (17 [18 19] <-- S - 8)))))))
  //     20 (21 (22 (23 (24 (25 (26 [27 28] <-- S - 8))))))
  //         ZZ
  //             31 (32 (33 (34 (35 [36 37] <-- S - 8))))
  //                 ZZ
  //                     ZZ
  //                         __ (00 [42* 43*] <-- S - 8)
  //                             __ [51 52] <-- S - 8
  // Empty

  // 10 (11 (12 (13 (14 (15 (16 (17 [18 19] <-- S - 8)))))))
  //     20 (21 (22 (23 (24 (25 (26 [27 28] <-- S - 8))))))
  //         ZZ
  //             31 (32 (33 (34 (__ [36* 37*] <-- S - 8))))
  //                 ZZ
  //                     ZZ
  //                         __ (00 [42 43] <-- S - 8)
  //                             __ [51 52] <-- S - 8
  // Empty

  // 10 (11 (12 (13 (14 (15 (16 (17 [18 19] <-- S - 8)))))))
  //     20 (21 (22 (23 (24 (25 (26 [27 28] <-- S - 8))))))
  //         ZZ
  //             31 (32 (33 (34 (__ [36* 37*] <-- S - 8))))
  //                 ZZ
  //                     ZZ
  //                         __ (00 [42 43] <-- S - 8)
  //                             __ [51 52] <-- S - 8
  // Empty

  // 10 (11 (12 (13 (14 (15 (16 (17 [18 19] <-- S - 8)))))))
  //     20 (21 (22 (23 (24 (25 (26 [27 28] <-- S - 8))))))
  //         ZZ
  //             31 (32 (33 (__ (__ [36* 37*] <-- S - 8))))
  //                 ZZ
  //                     ZZ
  //                         __ (00 [42 43] <-- S - 8)
  //                             __ [51 52] <-- S - 8
  // Empty

  // 10 (11 (12 (13 (14 (15 (16 (17 [18 19] <-- S - 8)))))))
  //     20 (21 (22 (23 (24 (25 (26 [27 28] <-- S - 8))))))
  //         ZZ
  //             31 (32 (33! (__ (__ [36 37] <-- S - 8))))
  //                 ZZ
  //                     ZZ
  //                         __ (00 [42 43] <-- S - 8)
  //                             __ [51 52] <-- S - 8
  // Empty

  // 10 (11 (12 (13 (14 (15 (16 (17 [18 19] <-- S - 8)))))))
  //     20 (21 (22 (23 (24 (25 (26 [27 28] <-- S - 8))))))
  //         ZZ
  //             31 (32! (33! (__ (__ [36 37] <-- S - 8))))
  //                 ZZ
  //                     ZZ
  //                         __ (00 [42 43] <-- S - 8)
  //                             __ [51 52] <-- S - 8
  // Empty

  // 10 (11 (12 (13 (14 (15 (16 (17 [18 19] <-- S - 8)))))))
  //     20 (21 (22 (23 (24 (25 (26 [27 28] <-- S - 8))))))
  //         ZZ
  //             31 (32! (33! (__ (__ [36 37] <-- S - 8))))
  //                 ZZ
  //                     ZZ
  //                         __ (00 [42 43] <-- S - 8)
  //                             __ [51 52] <-- S - 8
  // Empty

  // enum VectorNode[Size <: Int, A]:
  //   case Lead[Size <: Int, A](lead: A, tail: Either[Size =:= 1, VectorNode[Size - 1, A]]) extends VectorNode[Size, A]
  //   case Tail[Size <: Int, A](vector: Vector[Size, A])                                    extends VectorNode[Size, A]

  enum Node[Size <: Int, A]:
    case Skip[Size <: Int, A](a: A, next: Node[Size - 1, A])             extends Node[Size, A]
    case Zero[Size <: Int, A](lead: 0, next: Node[Size - 1, A])          extends Node[Size, A]
    case One[Size <: Int, A](lead: 1, next: Node[Size - 1, A])           extends Node[Size, A]
    case Tail[Size <: Int, A](tail: Either[Size =:= 0, Vector[Size, A]]) extends Node[Size, A]

  object Node:
    def map2[W <: Int](aNode: Node[W, A], bNode: Node[W, A])(f: (A, A) => A): Node[W, A] =
      aNode match
        case Skip(aA, aNext) =>
          bNode match // todo: other cases?
            case Skip(bA, bNext) => Skip(f(aA, bA), map2(aNext, bNext)(f))
        case Zero(0, aNext) =>
          bNode match // todo: other cases?
            case Zero(0, bNext) => Zero(0, map2(aNext, bNext)(f))
        case One(aLead, aNext) =>
          bNode match // todo: other cases?
            case One(1, bNext) => One(1, map2(aNext, bNext)(f))
        case Tail(aTail) =>
          bNode match // todo: other cases?
            case Tail(bTail) =>
              Tail(emap2(aTail, bTail) { (aVector, bVector) => Vector.map2(aVector, bVector)(f) })

    def combine[W <: Int](aNode: Node[W, A], bNode: Node[W - 1, A])(f: (A, A) => A): Node[W, A] =
      aNode match
        case Skip(a, next) =>
          Skip(a, map2(next, bNode)(f))
        case Zero(0, next) =>
          Zero(0, map2(next, bNode)(f))
        case One(1, next) =>
          One(1, map2(next, bNode)(f))
        case Tail(tail) =>
          val realTail = tail.getOrElse(throw RuntimeException("todo: realTal bad"))
          Zero(0, map2(Tail(l2(realTail.tail)), bNode)(f))

  import Node.*

  type W = 8
  val line2Lead: A        = 20
  val line2: Vector[W, A] = Vector.of(21, 22, 23, 24, 25, 26, 27, 28)

  enum Down[W <: Int, +A]:
    case Z[W <: Int, A]()                 extends Down[W, A]
    case L[W <: Int, A](node: Node[W, A]) extends Down[W, A]

  import Down.*

  val down3: Z[8, A] = Z() // zero
  val down4: Node[7, A] =
    One(1, Skip[6, A](32, Skip[5, A](33, Zero[4, A](0, Zero[3, A](0, Tail(Vector.of(36, 37).asRight))))))
  val down5: Z[6, A]    = Z() // zero
  val down6: Z[5, A]    = Z() // zero
  val down7: Node[4, A] = One(1, Zero[3, A](0, Tail(Vector.of(42, 43).asRight)))
  val down8: Node[3, A] = One(1, Tail(Vector.of(51, 52).asRight))

  enum NodeTrap[W <: Int, A]:
    case First[W <: Int, A](down: Down[W, A])                          extends NodeTrap[W, A]
    case Next[W <: Int, A](down: Down[W, A], next: NodeTrap[W - 1, A]) extends NodeTrap[W, A]

  import NodeTrap.*

  val nodeTrap: NodeTrap[8, A] =
    Next[8, A](down3, Next[7, A](L(down4), Next[6, A](down5, Next[5, A](down6, Next[4, A](L(down7), First(L(down8)))))))

  def l2[W <: Int, A](a: Either[W =:= 1, Vector[W - 1, A]]): Either[W - 1 =:= 0, Vector[W - 1, A]] = a.asInstanceOf
  def l3[W <: Int](a: =:=[W, 1]): =:=[W - 1, 0]                                                    = a.asInstanceOf

  def process[W <: Int](lead: A, tail: Either[W =:= 1, Vector[W - 1, A]], trap: NodeTrap[W - 1, A]): Node[W, A] =
    trap match
      case Next(down, nextTrap) =>
        down match
          case Z() =>
            val processed: Either[W =:= 1, Node[W - 1, A]] = tail.map(tail => process(tail.head, tail.tail, nextTrap))
            Skip(
              lead,
              processed.leftMap(wIs1 => Tail[W - 1, A](Left(l3(wIs1)))).merge,
            )
          case L(node) =>
            node match
              case Skip(a, next) =>
                val processed: Either[W =:= 1, Node[W - 1, A]] =
                  tail.map(tail => process(tail.head, tail.tail, nextTrap))
                Skip(
                  lead,
                  processed.leftMap(wIs1 => Tail[W - 1, A](Left(l3(wIs1)))).merge,
                )
              case Zero(0, next) =>
                val processed: Either[W =:= 1, Node[W - 1, A]] =
                  tail.map(tail => process(tail.head, tail.tail, nextTrap))
                val subtracted: Either[W =:= 1, Node[W - 1, A]] = processed.map { processed =>
                  Node.combine(processed, next) { (processedX, downX) =>
                    processedX - downX * lead
                  }
                }
                Zero(
                  lead = 0,
                  subtracted.leftMap(wIs1 => Tail[W - 1, A](Left(l3(wIs1)))).merge,
                )
              case One(1, next) =>
                val processed: Either[W =:= 1, Node[W - 1, A]] =
                  tail.map(tail => process(tail.head, tail.tail, nextTrap))
                val subtracted = processed.map { processed =>
                  Node.combine(processed, next) { (processedX, downX) =>
                    processedX - downX * lead
                  }
                }
                Zero(lead = 0, subtracted.leftMap(wIs1 => Tail[W - 1, A](Left(l3(wIs1)))).merge)
              case Tail(downTail) =>
                Zero[W, A](
                  lead = 0,
                  Tail[W - 1, A] {
                    emap2(downTail, l2(tail)) { (downVector, tailVector) =>
                      Vector.map2(downVector, tailVector) { (downX, tailX) =>
                        tailX - downX * lead
                      }
                    }
                  },
                )
      case First(down) =>
        down match
          case Z() =>
            Skip[W, A](
              lead,
              Tail[W - 1, A](l2(tail)),
            )
          case L(node) =>
            node match
              case Skip(a, next) =>
                Skip(
                  lead,
                  Tail[W - 1, A](l2(tail)),
                )
              case Zero(0, next) =>
                val processed: Node[W - 1, A] = Tail[W - 1, A](l2(tail))
                val subtracted: Node[W - 1, A] =
                  Node.combine(processed, next) { (processedX, downX) =>
                    processedX - downX * lead
                  }
                Zero(
                  lead = 0,
                  subtracted,
                )
              case One(1, next) =>
                val processed: Node[W - 1, A] = Tail[W - 1, A](l2(tail))
                val subtracted =
                  Node.combine(processed, next) { (processedX, downX) =>
                    processedX - downX * lead
                  }
                Zero(lead = 0, subtracted)
              case Tail(downTail) =>
                Zero[W, A](
                  lead = 0,
                  Tail[W - 1, A] {
                    emap2(downTail, l2(tail)) { (downVector, tailVector) =>
                      Vector.map2(downVector, tailVector) { (downX, tailX) =>
                        tailX - downX * lead
                      }
                    }
                  },
                )

  val result: Node[9, A] = process(line2Lead, Right(line2), nodeTrap)
}

@main def upTest = {
  println(UpSubtraction.result)
}
