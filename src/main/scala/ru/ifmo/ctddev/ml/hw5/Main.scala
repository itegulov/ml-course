package ru.ifmo.ctddev.ml.hw5

import java.io.File

import co.theasi.plotly._

/**
  * Created by Aleksei Latyshev on 11.11.2016.
  */
object Main extends App {
  val data = Data.parseData(new File(getClass.getResource("/non-parametric.csv").toURI))
  /*val plot = Plot().withScatter(data.map(_.x), data.map(_.y))
  draw(plot, "my-second-plot")
*/
  val nWR = NadarayaWatsonRegression.train(data, Kernels.quarticKernel, 3, Seq.fill(data.size)(1))
  /*val xs = 0D to 60D by 0.5
  val plot2 = Plot().withScatter(xs, xs.map(nWR))
  draw(plot2, "my-third-plot")*/
  val nWRWithLowess = NadarayaWatsonRegression.train(data, Kernels.rectangleKernel, 1,
    LowessRegression.getGammas(data, Kernels.rectangleKernel, 1, Kernels.triangleKernel))
}
