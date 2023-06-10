package utils

trait LinearInterpolation[A, Time]:
  def interpolate(point1: A, point2: A, time: Time): A

  extension (values: (A, A)) def interpolateBy(time: Time) = interpolate(values(0), values(1), time)

object LinearInterpolation:
  given LinearInterpolation[Double, Double] = (a, b, time) => a + (b - a) * time
