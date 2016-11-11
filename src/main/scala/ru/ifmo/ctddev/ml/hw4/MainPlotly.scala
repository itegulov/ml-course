package ru.ifmo.ctddev.ml.hw4

import co.theasi.plotly._

object MainPlotly {
  def main(args: Array[String]): Unit = {
    val xs = (0.0 to 2.0 by 0.1)
    val ys = xs.map { x => x*x }

    val plot = Plot().withScatter(xs, ys)

    draw(plot, "my-first-plot")
  }
}
