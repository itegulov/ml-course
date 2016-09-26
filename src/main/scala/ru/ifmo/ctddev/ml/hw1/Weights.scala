package ru.ifmo.ctddev.ml.hw1

object Weights {
  type Weight = (Point, Point) => Double

  def minkowski(p: Int): Weight =
    (f, s) => 1.0 / Distances.minkowski(p)(f, s)

  val manhattan: Weight = minkowski(1)

  val euclidean: Weight = minkowski(2)

  def const(f: Point, s: Point): Double = 1.0

  // TODO: do something with all this kernel stuff
}
