package ru.ifmo.ctddev.ml.hw5

import java.io.File

import co.theasi.plotly._
import ru.ifmo.ctddev.ml.hw5.Kernels.Kernel

/**
  * Created by Aleksei Latyshev on 11.11.2016.
  */
object Main extends App {

  val folderName: String = "hw5/"

  def t(k: Kernel, h: Double): Double => Double = {
    NadarayaWatsonRegression.train(data, k, h, Seq.fill(data.size)(1))
  }

  val data = Data.parseData(new File(getClass.getResource("/non-parametric.csv").toURI))
  val xs = 0D to 60D by 0.5

  def mse(algo: Double => Double): Double = {
    Math.sqrt(data.map {
      case Data(_, x, y) =>
        val myY = algo(x)
        (myY - y) * (myY - y)
    }.sum / data.size)
  }

  val kernels = Seq(
    (Kernels.rectangleKernel, "Rectangle"),
    (Kernels.triangleKernel, "Triangle"),
    (Kernels.qudraticKernel, "Quadratic"),
    (Kernels.quarticKernel, "Quartic"),
    (Kernels.gaussKernel, "Gauss")
  )

  var plot = Plot().withScatter(data.map(_.x), data.map(_.y), ScatterOptions().name("data").mode(ScatterMode.Marker))

  for ((kernel, name) <- kernels) {
    val algo = t(kernel, 1)
    println(s"Nadaraya Watson ($name h=1): ${mse(algo)}")
    plot = plot.withScatter(xs, xs.map(algo), ScatterOptions().name(name))
  }
  draw(plot, s"${folderName}kernel_comparing")

  val hs = Seq(0.1, 0.5, 1D, 2D, 3D)

  var plot2 = Plot().withScatter(data.map(_.x), data.map(_.y), ScatterOptions().name("data").mode(ScatterMode.Marker))
  for (h <- hs) {
    val algo = t(Kernels.gaussKernel, h)
    println(s"Nadaraya Watson (Gauss h=$h): ${mse(algo)}")
    plot2 = plot2.withScatter(xs, xs.map(algo), ScatterOptions().name(s"h = $h"))
  }
  draw(plot2, s"${folderName}h_comparing")

  val nWR = NadarayaWatsonRegression.train(data, Kernels.quarticKernel, 3, Seq.fill(data.size)(1))
  println(s"Nadaraya Watson (Qxuartic h=1): ${mse(nWR)}")
  val gammas: Seq[Double] = LowessRegression.getGammas(data, Kernels.quarticKernel, Kernels.quarticKernel, 10)
  val nWRWithLowess = NadarayaWatsonRegression.train(data, Kernels.quarticKernel, 3, gammas)
  println(s"Nadaraya Watson with lowess (quartic h=1): ${mse(nWRWithLowess)}")
  draw(Plot()
    .withScatter(data.map(_.x), data.map(_.y), ScatterOptions().name("data").mode(ScatterMode.Marker))
    .withScatter(xs, xs.map(nWR), ScatterOptions().name("without LOWESS"))
    .withScatter(xs, xs.map(nWRWithLowess), ScatterOptions().name("with LOWESS")),
    s"${folderName}LOWESS_comparing"
  )
}
