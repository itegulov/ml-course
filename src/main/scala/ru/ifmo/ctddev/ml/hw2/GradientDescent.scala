package ru.ifmo.ctddev.ml.hw2

/**
  * Created by Aleksei Latyshev on 26.09.2016.
  */
object GradientDescent extends LinearFitter {
  def cartesianProduct[T](xss: Seq[Seq[T]]): Seq[Seq[T]] = xss match {
    case h +: t => for(xh <- h; xt <- cartesianProduct( t)) yield xh +: xt
    case _ => Seq(Seq())
  }
  val firstStep : Double = 0.5
  val eps : Double = 0.0005
  override def fit(initialCoefficients: Seq[Double],
                     loss: Seq[Double] => Double): Seq[Double] = {
    def rec(ws : Seq[Double], step : Double) : Seq[Double] = {
      if (step < eps) {
        ws
      } else {
        rec(cartesianProduct(Seq.fill(ws.size)(Seq(-1, 0, 1))).map(_.zip(ws).map {
          case (x, y) => x * step + y
        }).minBy(loss), step / 2)
      }
    }
    rec(initialCoefficients, firstStep)
  }
}
