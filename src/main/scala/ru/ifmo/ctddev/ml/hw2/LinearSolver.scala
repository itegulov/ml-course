package ru.ifmo.ctddev.ml.hw2

trait LinearSolver {
  def solve(dataSet: Seq[Data]): Seq[Double]
}
