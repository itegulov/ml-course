package ru.ifmo.ctddev.ml.hw5

import java.io.File

import co.theasi.plotly._
import ru.ifmo.ctddev.ml.hw5.Kernels.Kernel

/**
  * Created by Aleksei Latyshev on 11.11.2016.
  */
object Main extends App {
  def t(k: Kernel, h: Double): Double => Double = {
    NadarayaWatsonRegression.train(data, k, h, Seq.fill(data.size)(1))
  }

  val data = Data.parseData(new File(getClass.getResource("/non-parametric.csv").toURI))
  /*val plot = Plot().withScatter(data.map(_.x), data.map(_.y))
  draw(plot, "my-second-plot")
*/
  val nWR = NadarayaWatsonRegression.train(data, Kernels.quarticKernel, 1, Seq.fill(data.size)(1))
  val gammas: Seq[Double] = LowessRegression.getGammas(data, Kernels.quarticKernel, Kernels.quarticKernel, 10)
  val nWRWithLowess = NadarayaWatsonRegression.train(data, Kernels.quarticKernel, 1, gammas)
  val xs = 0D to 60D by 0.5
  /*val p = Plot()
    .withScatter(xs, xs.map(nWR), ScatterOptions().name("nWR"))
    .withScatter(xs, xs.map(nWRWithLowess), ScatterOptions().name("nWRWithLowess"))
  draw(p, "my-third-plot")*/

  /*val fig = RowFigure(2)
    .plot(0) {
      Plot()
        .withScatter(data.map(_.x), data.map(_.y), ScatterOptions().name("data").mode(ScatterMode.Marker))
        .withScatter(xs, xs.map(t(Kernels.rectangleKernel, 1)), ScatterOptions().name("rectangle"))
        .withScatter(xs, xs.map(t(Kernels.triangleKernel, 1)), ScatterOptions().name("triangle"))
        .withScatter(xs, xs.map(t(Kernels.qudraticKernel, 1)), ScatterOptions().name("quadratic"))
        .withScatter(xs, xs.map(t(Kernels.quarticKernel, 1)), ScatterOptions().name("quartic"))
        .withScatter(xs, xs.map(t(Kernels.gaussKernel, 1)), ScatterOptions().name("gauss"))
    }
    .plot(1) {
      Plot()
        .withScatter(data.map(_.x), data.map(_.y), ScatterOptions().name("data").mode(ScatterMode.Marker))
        .withScatter(xs, xs.map(t(Kernels.gaussKernel, 0.1)), ScatterOptions().name("h = 0.1"))
        .withScatter(xs, xs.map(t(Kernels.gaussKernel, 0.5)), ScatterOptions().name("h = 0.5"))
        .withScatter(xs, xs.map(t(Kernels.gaussKernel, 1)), ScatterOptions().name("h = 1"))
        .withScatter(xs, xs.map(t(Kernels.gaussKernel, 2)), ScatterOptions().name("h = 2"))
        .withScatter(xs, xs.map(t(Kernels.gaussKernel, 3)), ScatterOptions().name("h = 3"))
    }*/
  //draw(fig, "kernels comparing")
  draw(Plot()
    .withScatter(xs, data.map(_.y))
    .withScatter(xs, xs.map(nWR), ScatterOptions().name("without LOWESS"))
    .withScatter(xs, xs.map(nWRWithLowess), ScatterOptions().name("with LOWESS")),
    "LOWESS comparing"
  )
}
