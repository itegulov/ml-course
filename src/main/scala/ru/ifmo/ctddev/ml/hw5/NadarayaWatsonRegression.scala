package ru.ifmo.ctddev.ml.hw5

import ru.ifmo.ctddev.ml.hw5.Kernels._

/**
  * Created by Aleksei Latyshev on 11.11.2016.
  */
object NadarayaWatsonRegression {
  def train(trainSet : Seq[Data], kernel : Kernel, windowSize : Double) : Double => Double = {
    x => {
      val k = trainSet.map(data => (kernel((x - data.x) / windowSize), data.y))
      k.map{ case (x, y) => x * y}.sum / k.map(_._1).sum
    }
  }
}
