package experimental

import matrix.{Vector, Matrix}
import scala.compiletime.ops.int.*
import matrix.Evidence
import math.aliases.*
import math.syntax.*
import math.Zero as ZeroT

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

  /*
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
   */

  // import Trap.*

  // val node0: Trapezoid[0, 0, 4, Int] = Empty()
  // val node1: Trapezoid[1, 1, 4, Int] = Node[1, 1, 4, Int](10, Vector.of(11, 12, 13), node0)
  // val node2: Trapezoid[2, 1, 4, Int] = Zero(node1)
  // val node3: Trapezoid[3, 2, 4, Int] = Node(30, Vector.of[Int](31), node2)

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

  sealed trait Node[Size <: Int, A]:
    def divideBy(lead: A)(using Div[A]): Node[Size, A]
    def toVector: Either[Size =:= 0, Vector[Size, A]]

  object Node:
    case class Skip[Size <: Int, A](a: A, next: Node[Size - 1, A]) extends Node[Size, A]:
      def divideBy(lead: A)(using Div[A]): Node[Size, A] = Skip(a / lead, next.divideBy(lead))
      def toVector: Either[Size =:= 0, Vector[Size, A]] =
        next.toVector match
          case Left(sIs1)  => Vector.of(a).asInstanceOf.asRight // todo: remove asInstanceOf
          case Right(tail) => l5(a +: tail).asRight

    case class Zero[Size <: Int, A](zero: A, next: Node[Size - 1, A]) extends Node[Size, A]:
      def divideBy(lead: A)(using Div[A]): Node[Size, A] = Zero(zero, next.divideBy(lead))
      def toVector: Either[Size =:= 0, Vector[Size, A]] =
        next.toVector match
          case Left(sIs1)  => Vector.of(zero).asInstanceOf.asRight // todo: remove asInstanceOf
          case Right(tail) => l5(zero +: tail).asRight

    case class Tail[Size <: Int, A](tail: Either[Size =:= 0, Vector[Size, A]]) extends Node[Size, A]:
      def divideBy(lead: A)(using Div[A]): Node[Size, A] = Tail(tail.map(_.map(_ / lead)))
      def toVector: Either[Size =:= 0, Vector[Size, A]]  = tail

    def apply[W <: Int, A](e: Either[W =:= 1, Node[W - 1, A]]): Node[W - 1, A] = e match
      case Left(wIs1)  => Tail(Left(l3(wIs1)))
      case Right(node) => node

    // todo: is it possible to skip => base cases?
    def map2[W <: Int, A](base: Node[W, A], down: Node[W, A])(f: (A, A) => A): Node[W, A] =
      base match
        case Skip(a, next) =>
          down match
            case Skip(_, downNext) => Skip(a, map2(next, downNext)(f))
            case Zero(zero, next)  => base
            case Tail(tail)        => base

        case Zero(zero, next) =>
          down match
            case Zero(_, downNext) => Zero(zero, map2(next, downNext)(f))
            case Skip(a, next)     => base
            case Tail(tail)        => base

        case Tail(tail) =>
          down match
            case Tail(downTail) =>
              Tail(emap2(tail, downTail) { (tailVector, downVector) => Vector.map2(tailVector, downVector)(f) })
            case Skip(a, next)    => base
            case Zero(zero, next) => base

  // enum Node[Size <: Int, +A]:
  //   case Skip(a: A, next: Node[Size - 1, A])             extends Node[Size, A]
  //   case Zero(lead: 0, next: Node[Size - 1, A])          extends Node[Size, A]
  //   case Tail(tail: Either[Size =:= 0, Vector[Size, A]]) extends Node[Size, A]

  // enum Down[Size <: Int, A]:
  //   case ZeroLine()
  //   case NodeLine(node: Node[Size, A])

  // enum NodeTrap[W <: Int, A]:
  //   case First(down: Down[W, A])                          extends NodeTrap[W, A]
  //   case Next(down: Down[W, A], next: NodeTrap[W - 1, A]) extends NodeTrap[W, A]

  case object ZeroLine
  type ZeroLine = ZeroLine.type

  enum NodeTrap2[W <: Int, A]:
    case First(down: ZeroLine | Node.Tail[W, A])
    case Next(down: ZeroLine | Node.Skip[W, A] | Node.Zero[W, A], next: NodeTrap2[W - 1, A])

  /*
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

  type W = 9
  val line2Lead: A        = 20
  val line2: Vector[W, A] = Vector.of(21, 22, 23, 24, 25, 26, 27, 28)

  enum Down[W <: Int, +A]:
    case Z[W <: Int, A]()                 extends Down[W, A]
    case L[W <: Int, A](node: Node[W, A]) extends Down[W, A]

  val down3: Node[7, A] = FullZero() // zero
  val down4: Node[6, A] = Skip[6, A](32, Skip[5, A](33, Zero[4, A](0, Zero[3, A](0, Tail(Vector.of(36, 37).asRight)))))
  val down5: Node[5, A] = FullZero() // zero
  val down6: Node[4, A] = FullZero() // zero
  val down7: Node[3, A] = Zero[3, A](0, Tail(Vector.of(42, 43).asRight))
  val down8: Node[2, A] = Tail(Vector.of(51, 52).asRight)

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
   */

  def l2[W <: Int, A](a: Either[W =:= 1, Vector[W - 1, A]]): Either[W - 1 =:= 0, Vector[W - 1, A]] = a.asInstanceOf
  def l3[W <: Int](a: =:=[W, 1]): =:=[W - 1, 0]                                                    = a.asInstanceOf
  def l4[W <: Int, A](a: Either[W - 1 =:= 1, Vector[W - 1 - 1, A]]): Either[W - 2 =:= 0, Vector[W - 2, A]] =
    a.asInstanceOf
  def l5[S <: Int, A](a: Vector[S - 1 + 1, A]): Vector[S, A] = a.asInstanceOf

  import Node.*
  import NodeTrap2.*

  def process[W <: Int, A: Div: Mul: Sub: ZeroT](
    base: Vector[W, A],
    trap: NodeTrap2[W - 1, A],
  ): Node[W, A] =
    trap match
      case NodeTrap2.First(down) =>
        down match
          case ZeroLine =>
            val baseLead: A   = base.head
            val maybeBaseTail = base.tail
            Skip[W, A](baseLead, Tail[W - 1, A](l2(maybeBaseTail)))
          case Tail(maybeDownTail) =>
            val baseLead: A   = base.head
            val maybeBaseTail = l2(base.tail)
            val upSubtracted =
              emap2(maybeBaseTail, maybeDownTail) { (baseTail, downTail) =>
                Vector.map2(baseTail, downTail) { (baseX, downX) => baseX - downX * baseLead }
              }
            Zero[W, A](ZeroT.of[A], Tail[W - 1, A](upSubtracted))

      case NodeTrap2.Next(down, nextTrap) =>
        down match
          case ZeroLine =>
            val baseLead: A    = base.head
            val maybeBaseTail  = base.tail
            val maybeProcessed = maybeBaseTail.map { baseTail => process(baseTail, nextTrap) }
            Skip[W, A](baseLead, Node(maybeProcessed))
          case Skip(_, _) =>
            val baseLead: A    = base.head
            val maybeBaseTail  = base.tail
            val maybeProcessed = maybeBaseTail.map { baseTail => process(baseTail, nextTrap) }
            Skip[W, A](baseLead, Node(maybeProcessed))
          case zeroNode @ Zero(_, _) =>
            val baseLead: A   = base.head
            val maybeBaseTail = base.tail
            val maybeSubtracted: Either[W =:= 1, Node[W - 1, A]] = maybeBaseTail.map { baseTail =>
              val processed = process(baseTail, nextTrap)
              Node.map2(processed, zeroNode) { (baseX, downX) => baseX - downX * baseLead }
            }
            Zero(ZeroT.of[A], Node(maybeSubtracted))

  type A = Int

  given Div[A] = _ / _ // todo: inacurate, for test
  type W = 9
  val line2Lead: A            = 20
  val line2: Vector[W - 1, A] = Vector.of(21, 22, 23, 24, 25, 26, 27, 28)

  val down3             = ZeroLine // zero
  val down4: Skip[6, A] = Skip[6, A](32, Skip[5, A](33, Zero[4, A](0, Zero[3, A](0, Tail(Vector.of(36, 37).asRight)))))
  val down5             = ZeroLine // zero
  val down6             = ZeroLine // zero
  val down7: Zero[3, A] = Zero[3, A](0, Tail(Vector.of(42, 43).asRight))
  val down8: Tail[2, A] = Tail(Vector.of(51, 52).asRight[2 =:= 0])

  val nodeTrap: NodeTrap2[7, A] =
    Next[7, A](down3, Next[6, A](down4, Next[5, A](down5, Next[4, A](down6, Next[3, A](down7, First(down8))))))

  val result1: Node[8, A] = process[8, A](line2, nodeTrap)

  val result = process[6, A](
    Vector.of(32, 33, 34, 35, 36, 37),
    Next[5, A](down5, Next[4, A](down6, Next[3, A](down7, First(down8)))),
  )
}

@main def upTest = {
  println(UpSubtraction.result1)
  println(UpSubtraction.result1.toVector)
  println(UpSubtraction.result)
  println(UpSubtraction.result.toVector)
}
