package ru.ifmo.ctddev.ml.hw2

trait LinearFitter {
  def fit(initialCoefficients: Seq[Double], loss: Seq[Double] => Double): Seq[Double]
}
