package ru.ifmo.ctddev.ml.hw2

trait LinearSolver {
  def solve(dataSet: Seq[Data],
            initialCoefficients: Seq[Double],
            loss: Seq[Double] => Double): Seq[Double]
}
