package ru.ifmo.ctddev.ml.hw2

/**
  * Created by Aleksei Latyshev on 26.09.2016.
  */
object GradientDescent {
  def gradientDescent(houses: Seq[HouseWithPrice], ww1: Double, ww2: Double): (Double, Double) = {
    val q: (Double, Double) => Double =
      (w1, w2) => houses.map {
        case HouseWithPrice(house, price) => Math.pow(house.area * w1 + house.rooms * w2 - price, 2)
      }.sum
    def rec : (Double, Double) => (Double) => (Double, Double) =
      (w1, w2) => (step) => if (step < 0.00005) (w1, w2)
      else rec.tupled((for (x <- Seq(-1, 0, 1); y <- Seq(-1, 0, 1)) yield (x, y))
        .map { case (i, j) => (w1 + i * step, w2 + j * step) }
        .map { case (i, j) => (q(i, j), (i, j)) }.min._2) (step / 2)
    rec(ww1, ww2)(100000)



/*    var step : Double = 1 << 20
    var w1 = ww1
    var w2 = ww2
    for (i <- 0 to 100) {
      var nw1 : Double = 0
      var nw2 : Double = 0
      var curQ : Double = 1000000
      for (j <- -1 to 1) {
        for (k <- -1 to 1) if (j != 0 || k != 0) {
          val nww1 = w1 + step * j
          val nww2 = w2 + step * k
          val x = q(nww1, nww2)
          if (x < curQ) {
            curQ = x
            nw1 = nww1
            nw2 = nww2
          }
        }
      }
      w1 = nw1
      w2 = nw2
      step /= 2
    }
    println(q(w1, w2))
    (w1, w2)*/
  }
}
