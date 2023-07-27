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

  sealed trait Node[Size <: Int, +A]:
    def toVector: Either[Size =:= 0, Vector[Size, A]]

  object Node:
    sealed trait Artificial[Size <: Int, A](value: A, next: Node[Size - 1, A]) extends Node[Size, A]:
      def toVector: Either[Size =:= 0, Vector[Size, A]] =
        next.toVector match
          case Left(sIs1)  => Vector.of(value).asInstanceOf.asRight // todo: remove asInstanceOf
          case Right(tail) => l5(value +: tail).asRight

    case class Skip[Size <: Int, A](a: A, next: Node[Size - 1, A])    extends Artificial[Size, A](a, next)
    case class Zero[Size <: Int, A](zero: A, next: Node[Size - 1, A]) extends Artificial[Size, A](zero, next)

    case class Tail[Size <: Int, A](tail: Either[Size =:= 0, Vector[Size, A]]) extends Node[Size, A]:
      def toVector: Either[Size =:= 0, Vector[Size, A]] = tail

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

  case object ZeroLine
  type ZeroLine = ZeroLine.type

  enum NodeTrap[W <: Int, A]:
    case First(down: ZeroLine | Node.Tail[W, A])
    case Next(down: ZeroLine | Node.Artificial[W, A], next: NodeTrap[W - 1, A])

  def l2[W <: Int, A](a: Either[W =:= 1, Vector[W - 1, A]]): Either[W - 1 =:= 0, Vector[W - 1, A]] = a.asInstanceOf
  def l3[W <: Int](a: =:=[W, 1]): =:=[W - 1, 0]                                                    = a.asInstanceOf
  def l5[S <: Int, A](a: Vector[S - 1 + 1, A]): Vector[S, A]                                       = a.asInstanceOf

  import Node.*
  import NodeTrap.*

  def subtractBy[A: Mul: Sub](coefficient: A)(base: A, down: A): A =
    base - down * coefficient

  def process[W <: Int, A: Mul: Sub: ZeroT](
    base: Vector[W, A],
    trap: NodeTrap[W - 1, A],
  ): Node[W, A] =
    val baseLead: A   = base.head
    val maybeBaseTail = base.tail
    trap match
      case NodeTrap.First(down) =>
        down match
          case ZeroLine => Skip[W, A](baseLead, Tail[W - 1, A](l2(maybeBaseTail)))
          case Tail(maybeDownTail) =>
            val upSubtracted =
              emap2(l2(maybeBaseTail), maybeDownTail) { (baseTail, downTail) =>
                Vector.map2(baseTail, downTail)(subtractBy(baseLead))
              }
            Zero[W, A](ZeroT.of[A], Tail[W - 1, A](upSubtracted))

      case NodeTrap.Next(down, nextTrap) =>
        val maybeProcessed = maybeBaseTail.map { baseTail => process(baseTail, nextTrap) }
        def onArtificial(node: Node.Artificial[W - 1, A]): Node[W, A] =
          Zero[W, A](ZeroT.of[A], Node(maybeProcessed.map(Node.map2(_, node)(subtractBy(baseLead)))))
        down match
          case ZeroLine          => Skip[W, A](baseLead, Node(maybeProcessed))
          case node @ Skip(_, _) => onArtificial(node)
          case node @ Zero(_, _) => onArtificial(node)

  type A = Int

  type W = 9
  val line2Lead: A            = 20
  val line2: Vector[W - 1, A] = Vector.of(21, 22, 23, 24, 25, 26, 27, 28)

  val down3             = ZeroLine // zero
  val down4: Skip[6, A] = Skip[6, A](32, Skip[5, A](33, Zero[4, A](0, Zero[3, A](0, Tail(Vector.of(36, 37).asRight)))))
  val down5             = ZeroLine // zero
  val down6             = ZeroLine // zero
  val down7: Zero[3, A] = Zero[3, A](0, Tail(Vector.of(42, 43).asRight))
  val down8: Tail[2, A] = Tail(Vector.of(51, 52).asRight[2 =:= 0])

  val nodeTrap: NodeTrap[7, A] =
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
