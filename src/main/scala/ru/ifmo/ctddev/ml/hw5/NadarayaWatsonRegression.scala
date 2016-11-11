package ru.ifmo.ctddev.ml.hw5

import ru.ifmo.ctddev.ml.hw5.Kernels._

/**
  * Created by Aleksei Latyshev on 11.11.2016.
  */
object NadarayaWatsonRegression {
  def train(trainSet : Seq[Data], kernel : Kernel, windowSize : Double, gamma : Seq[Double]) : Double => Double = {
    x => {
      val k = trainSet.zip(gamma).map{ case (data, g) => (g * kernel((x - data.x) / windowSize), data.y)}
      k.map{ case (xx, yy) => xx * yy}.sum / k.map(_._1).sum
    }
  }
}
