package utils

trait Product[-A, -B, C]:
  def product(x: A, y: B): C

  extension (x: A) def ***(y: B): C = product(x, y)

object Product:
  given Product[Int, Int, Int]       = _ * _
  given Product[String, Int, String] = _ * _
