package ru.ifmo.ctddev.ml.hw5

import java.io.File

/**
  * Created by Aleksei Latyshev on 11.11.2016.
  */
object Main extends App {
  val data = Data.parseData(new File(getClass.getResource("/non-parametric.csv").toURI))
  val nWR = NadarayaWatsonRegression.train(data, Kernels.rectangleKernel, 1)

}
