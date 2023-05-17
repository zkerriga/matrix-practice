package utils

trait ProductA[A, -B]:
  def product(x: A, y: B): A

object ProductA:
  given ProductA[Int, Int]    = _ * _
  given ProductA[String, Int] = _ * _
