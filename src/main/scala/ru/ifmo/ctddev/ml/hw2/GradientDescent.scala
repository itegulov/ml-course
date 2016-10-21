package ru.ifmo.ctddev.ml.hw2

import scala.util.control.Breaks

object GradientDescent {
  val eps = 0.0000001

  def fit(initialCoefficients: Seq[Double], lossDerivative: Seq[Double] => Seq[Double]): Seq[Double] = {
    var currentCoefficients = initialCoefficients
    Breaks.breakable {
      while (true) {
        val diff = lossDerivative(currentCoefficients).map(-_)
        if (diff.map(Math.pow(_, 2)).sum < eps * diff.size) {
          Breaks.break()
        }
        currentCoefficients = currentCoefficients.zip(diff).map {
          case (f, s) => f + s
        }
      }
    }
    currentCoefficients
  }
}
