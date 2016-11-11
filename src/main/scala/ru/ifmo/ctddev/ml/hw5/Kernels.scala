package ru.ifmo.ctddev.ml.hw5

/**
  * Created by Aleksei Latyshev on 11.11.2016.
  */
object Kernels {
  type Kernel = Double => Double
  val rectangleKernel : Kernel = x => if (Math.abs(x) <= 1) 0.5 else 0
  val triangleKernel : Kernel = x => rectangleKernel(x) * (1 - Math.abs(x))
  val qudraticKernel : Kernel = x => rectangleKernel(x) * (1 - x * x)
  val quarticKernel : Kernel = x => rectangleKernel(x) * (1 - x * x) * (1 - x * x)
  val gaussKernel : Kernel = x => Math.exp(-2 * x * x)
}
