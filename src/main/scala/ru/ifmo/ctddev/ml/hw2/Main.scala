package ru.ifmo.ctddev.ml.hw2

import java.awt.Color

import breeze.numerics._
import breeze.linalg._
import breeze.plot._

object Main extends App {
  val samples = HouseWithPrice.parseData(getClass.getResourceAsStream("/prices.txt"))
  val (w0, w1) = GeneticAlgorithm(samples, 0, 0)
  println(GeneticAlgorithm.loss(samples)(w0, w1))

  val pointsFigure = Figure()
  val pointsPlot = pointsFigure.subplot(0)
  pointsPlot += scatter(samples.map(_.house.area), samples.map(_.house.rooms), _ => 20.0, colors = _ => Color.red)
  pointsPlot.title = "Data points"

  println(w0 + " " + w1)

  val x = linspace(0, 4000)
  val y = linspace(0, 4000 * w0 / w1)
  pointsPlot += plot(x, y, '.')
}
