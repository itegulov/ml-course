package ru.ifmo.ctddev.ml.hw2

/**
  * Created by Aleksei Latyshev on 26.09.2016.
  */
object GradientDescent {
//  def gradientDescent(houses: Seq[HouseWithPrice], ww1: Double, ww2: Double): (Double, Double) = {
//    def q(w1: Double, w2: Double): Double =
//      houses.map {
//        case HouseWithPrice(house, price) => Math.pow(house.area * w1 + house.rooms * w2 - price, 2)
//      }.sum
//    def rec: (Double, Double) => (Double) => (Double, Double) =
//      (w1, w2) => (step) => if (step < 0.00005) (w1, w2)
//      else rec.tupled((for (x <- Seq(-1, 0, 1); y <- Seq(-1, 0, 1)) yield (x, y))
//        .map { case (i, j) => (w1 + i * step, w2 + j * step) }
//        .map { case (i, j) => (q(i, j), (i, j)) }.min._2)(step / 2)
//    rec(ww1, ww2)(100000)
//  }

  def apply(houses: Seq[NormalizedHouseWithPrice], w0: Double, w1: Double): (Double, Double) = {
    var ww0 = w0
    var ww1 = w1
    val thisLoss: (Double, Double) => Double = GeneticAlgorithm.loss(houses)
    var step = 1
    while (step > 0.0000005) {
      val forward = ww0 + step
      val backward = ww0 - step
      ww0 = if (thisLoss(forward, w1) < thisLoss(backward, w1)) forward else backward
      step /= 2
    }

    step = 1
    while (step > 0.0000005) {
      val forward = ww1 + step
      val backward = ww1 - step
      ww1 = if (thisLoss(ww0, forward) < thisLoss(ww0, backward)) forward else backward
      step /= 2
    }
    (ww0, ww1)
  }
}
