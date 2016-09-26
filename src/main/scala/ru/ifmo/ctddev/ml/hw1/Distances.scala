package ru.ifmo.ctddev.ml.hw1

import java.lang.Math.pow

object Distances {
  type Distance = (Point, Point) => Double

  def minkowski(p: Int): Distance =
    (f, s) => pow(pow(f.x - s.x, p) + pow(f.y - s.y, p), 1.0D / p)

  def generalizedMinkowski(p: Int)(w1: Double, w2: Double): Distance =
    (f, s) => pow(pow(f.x - s.x, p) * w1 + pow(f.y - s.y, p) * w2, 1.0D / p)

  val manhattan: Distance = minkowski(1)

  val euclidean: Distance = minkowski(2)

  val generalizedManhattan: (Double, Double) => Distance = generalizedMinkowski(1)

  val generalizedEuclidean: (Double, Double) => Distance = generalizedMinkowski(2)

  def kernelize(d : Distance, kernel : Double => Double) : Distance =
    (f, s) => kernel(d(f, s))
}
