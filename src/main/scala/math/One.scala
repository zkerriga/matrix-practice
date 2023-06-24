package math

trait One[+A](val value: A)

object One:
  def apply[A](one: A): One[A] = new One(one) {}

  def of[A](using one: One[A]): A = one.value

  given One[Double] = One(1.0)
  given One[Int]    = One(1)
