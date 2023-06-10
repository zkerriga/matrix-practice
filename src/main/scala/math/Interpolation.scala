package math

trait Interpolation[A, -Time]:
  def interpolate(from: A, to: A, time: Time): A

  extension (values: (A, A)) def interpolateBy(time: Time): A = interpolate(values(0), values(1), time)

object Interpolation:
  given Interpolation[Double, Double] = (from, to, time) => from + (to - from) * time
