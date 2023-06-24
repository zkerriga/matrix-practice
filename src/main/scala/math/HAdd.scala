package math

trait HAdd[-A, -B, +C]:
  def add(a: A, b: B): C

object HAdd:
  type Add[A] = HAdd[A, A, A]

  extension [A](a: A) infix def +[B, C](b: B)(using ha: HAdd[A, B, C]): C = ha.add(a, b)

  given Add[Double] = _ + _
  given Add[Int]    = _ + _
