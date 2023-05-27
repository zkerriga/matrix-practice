package utils

trait LinearInterpolation[A, Time]:
  def interpolate(point1: A, point2: A, time: Time): A

  extension (values: (A, A)) def interpolateBy(time: Time) = interpolate(values(0), values(1), time)

object LinearInterpolation:
  given LinearInterpolation[Double, Double] = (a, b, time) => a + (b - a) * time

  given [Size <: Int, A, Time](using LinearInterpolation[A, Time]): LinearInterpolation[matrix.Vector[Size, A], Time] =
    (v1, v2, time) => matrix.Vector.map2(v1, v2)((value1, value2) => (value1, value2).interpolateBy(time))

  given [Weight <: Int, Height <: Int, A, Time](using
    LinearInterpolation[A, Time]
  ): LinearInterpolation[matrix.Matrix[Weight, Height, A], Time] =
    (m1, m2, time) => matrix.Matrix.map2(m1, m2)((value1, value2) => (value1, value2).interpolateBy(time))
