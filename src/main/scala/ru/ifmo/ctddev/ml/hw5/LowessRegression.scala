package ru.ifmo.ctddev.ml.hw5

import ru.ifmo.ctddev.ml.hw5.Kernels.Kernel

/**
  * Created by Aleksei Latyshev on 11.11.2016.
  */
object LowessRegression {
  val eps = 1e-6

  def getGammas(trainSet: Seq[Data], kernel: Kernel, kernel2: Kernel, k: Int): Seq[Double] = {
    def rec(cur: Seq[Double]): Seq[Double] = {
      val windowSize = trainSet.zipWithIndex.map { case (data, i) =>
        trainSet.zipWithIndex
          .filter { case (_, j) => i != j }
          .map { case (d, _) => Math.abs(d.x - data.x) }
          .sorted.toIndexedSeq(k)
      }
      val a = trainSet.zipWithIndex.map { case (di, i) =>
        val k = trainSet.zipWithIndex.zip(cur)
          .filter { case ((_, j), _) => i != j }
          .map { case ((data, _), gamma) =>
            (gamma * kernel((data.x - di.x) / windowSize(i)), data.y)
          }
        k.map { case (x, y) => x * y }.sum / k.map(_._1).sum
      }
      val e = a.zip(trainSet.map(_.y)).map { case (x, y) => Math.abs(x - y) }
      val magic = e.sorted.toIndexedSeq(e.size / 2) * 6
      val next = e.map(x => kernel2(x / magic))
      if (next.zip(cur).map { case (x, y) => (x - y) * (x - y) }.sum < eps) {
        next
      } else {
        rec(next)
      }
    }
    rec(Seq.fill(trainSet.size)(1D))
  }
}
