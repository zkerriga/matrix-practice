package matrix

import utils.{Product, Semigroup}

import scala.compiletime.ops.int.*
import scala.compiletime.ops.boolean.*

import scala.collection.immutable.Vector as Vec

trait Vector[Size <: Int, +A](size: Size)(using Size >= 0 =:= true):
  def apply[i <: Int & Singleton](index: i)(using i ?>= 0, i ?< Size): A

  def +[A1 >: A: Semigroup](vector: Vector[Size, A1]): Vector[Size, A1]
  def *[B, C](scalar: B)(using Product[A, B, C]): Vector[Size, C]

object Vector:
  abstract class Tabulate[size <: Int, A]:
    def run(x: Int): (x.type ?< size, x.type ?>= 0) ?=> A

  def tabulate[size <: Int, A](size: size)(
    f: Tabulate[size, A]
  )(using size ?>= 0): Vector[size, A] =
    Impl[size, A](size, Vec.tabulate(size)(x => f.run(x)(using believeMe, believeMe)))

  private[this] def believeMe[P <: Boolean]: P =:= true = <:<.refl.asInstanceOf[P =:= true]
  private class Impl[Size <: Int, +A](val size: Size, vec: Vec[A])(using Size ?>= 0)
      extends Vector[Size, A](size):

    def apply[index <: Int](
      index: index
    )(using index ?>= 0, index ?< Size): A = vec(index)

    def +[A1 >: A: Semigroup](other: Vector[Size, A1]): Vector[Size, A1] =
      tabulate(size)(
        new:
          def run(i: Int) = vec(i: i.type) |+| other(i: i.type)
      )

    def *[B, C](scalar: B)(using Product[A, B, C]): Vector[Size, C] =
      Impl[Size, C](size, vec.map(_ *** scalar))

    override def toString: String = vec.mkString("[", ", ", "]")
  end Impl

  type Make[T <: Tuple] = Vector[Tuple.Size[T], Tuple.Union[T]]

  def make(tuple: Tuple): Make[tuple.type] =
    Impl(tuple.size, tuple.toList.toVector)(using believeMe)

  def of[Size <: Int, T](tuple: Tuple)(using
    uev: Tuple.Union[tuple.type] <:< T,
    szev: Tuple.Size[tuple.type] =:= Size,
  ): Vector[Size, T] =
    val onSize = szev.liftCo[[x] =>> Vector[x & Int, T]]
    val onElem = uev.liftCo[[x] =>> Vector[Tuple.Size[tuple.type], x]]
    onSize.compose(onElem)(make(tuple))

//   def apply(tuple: NonEmptyTuple): Vector[Tuple.Size[tuple.type], Tuple.Union[tuple.type]] =
//     Impl(tuple.size, tuple.toArray.asInstanceOf[Array[Tuple.Union[tuple.type]]])

/*
  def make[A](tuple: NonEmptyTuple)(using
    Tuple.Union[tuple.type] =:= A
  ): Vector[Tuple.Size[tuple.type], A] =
    apply(tuple).asInstanceOf[Vector[Tuple.Size[tuple.type], A]]

  def makeSized[Size <: Int, A](
    tuple: NonEmptyTuple
  )(using Tuple.Union[tuple.type] =:= A, Tuple.Size[tuple.type] =:= Size): Vector[Size, A] =
    apply(tuple).asInstanceOf[Vector[Size, A]]
 */
// @main def test = {
//   import scala.compiletime.ops.int.*
//   import scala.compiletime.ops.any.*
//   import scala.compiletime.ops.boolean.*

//   def test[Size <: Int & Singleton](size: Size)(using Size > 0 =:= true): Size = size

//   def test2[Size <: Int & Singleton, Tup <: Tuple](
//     t: Tup
//   )(using Size > 0 =:= true, Tuple.Size[Tup] =:= Size): Unit = ()

//   class Partial[Size <: Int & Singleton]() {
//     def apply[Tup <: Tuple](t: Tup)(using Tuple.Size[Tup] =:= Size): Unit = ???
//   }
//   def test3[Size <: Int & Singleton]: Partial[Size] = Partial[Size]

//   def test4[Tup <: Tuple](t: Tup): Tuple.Size[Tup] = t.size

//   /*type Concat[X <: Tuple, +Y <: Tuple] <: Tuple = X match {
//     case EmptyTuple => Y
//     case x1 *: xs1 => x1 *: Concat[xs1, Y]
//   }*/

//   type AllElementsOfType[X <: Tuple, A] <: Boolean = X match {
//     case EmptyTuple => true
//     case x1 *: xs1  => (x1 == A) && AllElementsOfType[xs1, A]
//   }

// //  def test5[Tup <: Tuple](t: Tup)(using AllElementsOfType[Tup, Tuple.Head[Tup]] =:= true): (Tuple.Size[Tup], List[Tuple.Union[Tup]]) = (t.size, t.toList)
//   def test6[Tup <: Tuple](t: Tup): (Tuple.Size[Tup], List[Tuple.Union[Tup]]) =
//     (t.size, t.toList.asInstanceOf[List[Tuple.Union[Tup]]])

//   def test7(t: Tuple): (Tuple.Size[t.type], List[Tuple.Union[t.type]]) = (t.size, t.toList)

//   def test8(t1: Tuple, t2: Tuple)(using Tuple.Size[t1.type] =:= Tuple.Size[t2.type]): Unit = ()
// //  val result = test2[2, (Char, Char)](('a', 'b'))
// //  val result = test4(('a', 'b'))
// //  val result = test5(('a', 'b', 'c'))
// //  val result: (3, List[Char]) = test6(('a', 'b', 'c'))
// //  val result: (3, List[Char]) = test7(('a', 'b', 'c'))

// //  println(s"result = ${result}")

//   val vector3: Vector[3, Double] = Vector.make((1.2, 4.0, 1.0))

// //   val vector2: Vector[3, Double] = Vector.make[Double]((1.0, 3.5, 5.4))
// //   val vector1: Vector[3, Double] = Vector.makeSized[3, Double]((1.0, 3.5, 5.4))

//   println(vector3)
// }
