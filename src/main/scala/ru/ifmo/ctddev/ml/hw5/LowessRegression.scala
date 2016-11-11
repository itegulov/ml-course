package ru.ifmo.ctddev.ml.hw5

import ru.ifmo.ctddev.ml.hw5.Kernels.Kernel

/**
  * Created by Aleksei Latyshev on 11.11.2016.
  */
object LowessRegression {
  val eps = 1e-6
  def getGammas(trainSet: Seq[Data], kernel: Kernel, windowSize: Double): Seq[Double] = {
    def rec(cur: Seq[Double]): Seq[Double] = {
      val next = trainSet.zipWithIndex.map { case (di, i) =>
        val k = trainSet.zipWithIndex.zip(cur).filter { case ((_, j), _) => i != j }.map {
          case ((data, _), gamma) => (gamma * kernel((data.x - di.x) / windowSize), data.y)
        }
        k.map { case (x, y) => x * y }.sum / k.map(_._1).sum
      }
      if (next.zip(cur).map{ case (x, y) => (x - y) * (x - y)}.sum < eps) {
        next
      } else {
        rec(next)
      }
    }
    rec(Seq.fill(trainSet.size)(1D))
  }
}
