package ru.ifmo.ctddev.ml.hw2

import java.awt.Color

import breeze.numerics._
import breeze.linalg._
import breeze.plot._

object Main extends App {
  val samples = HouseWithPrice.parseData(getClass.getResourceAsStream("/prices.txt"))
  val (wgd0, wgd1) = GradientDescent(samples, 100, 10000)
  val (wga0, wga1) = GeneticAlgorithm(samples, 0, 0)

  println(GeneticAlgorithm.loss(samples)(wgd0, wgd1))
  println(GeneticAlgorithm.loss(samples)(wga0, wga1))

  println(wgd0 + " " + wgd1)
  println(wga0 + " " + wga1)

  val areaFigure = Figure()
  val areaPlot = areaFigure.subplot(0)
  areaPlot += scatter(samples.map(_.house.area), samples.map(_.price), _ => 20.0, colors = _ => Color.red)
  areaPlot.title = "Area"

  val x = linspace(0, 4000)
  areaPlot += plot(x, x.map(_ * wgd0), '.')
  areaPlot += plot(x, x.map(_ * wga0), '.')

  val roomsFigure = Figure()
  val roomsPlot = roomsFigure.subplot(0)
  roomsPlot += scatter(samples.map(_.house.rooms), samples.map(_.price), _ => 0.1, colors = _ => Color.red)
  roomsPlot.title = "Rooms"

  val y = linspace(0, 5)
  roomsPlot += plot(y, y.map(_ * wgd1), '.')
  roomsPlot += plot(y, y.map(_ * wga1), '.')
}
