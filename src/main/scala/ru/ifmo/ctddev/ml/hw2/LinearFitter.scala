package ru.ifmo.ctddev.ml.hw2

trait LinearFitter {
  def fit(dataSet: Seq[Data],
            initialCoefficients: Seq[Double],
            loss: Seq[Double] => Double): Seq[Double]
}
