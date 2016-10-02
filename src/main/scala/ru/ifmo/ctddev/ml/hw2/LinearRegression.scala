package ru.ifmo.ctddev.ml.hw2

object LinearRegression {
  def apply(coefficients: Seq[Double]): Seq[Double] => Double =
    features => features.zip(coefficients).map { case (f, s) => f * s }.sum
}
