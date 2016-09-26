package ru.ifmo.ctddev.ml.hw2

import java.awt.Color

import breeze.numerics._
import breeze.linalg._
import breeze.plot._

import scala.io.StdIn

object Main extends App {
  val initialSamples = HouseWithPrice.parseData(getClass.getResourceAsStream("/prices.txt"))
  val maxArea = initialSamples.maxBy(_.house.area).house.area
  val maxRooms = initialSamples.maxBy(_.house.rooms).house.rooms
  val maxPrice = initialSamples.maxBy(_.price).price
  println(s"$maxArea $maxRooms $maxPrice")
  val samples = initialSamples.map {
    case HouseWithPrice(House(area, rooms), price) =>
      NormalizedHouseWithPrice(
        NormalizedHouse(area.toDouble / maxArea, rooms.toDouble / maxRooms),
        price.toDouble / maxPrice
      )
  }
  val (wgd0, wgd1) = GradientDescent(samples, 0, 0)
  val (wga0, wga1) = GeneticAlgorithm(samples, 0, 0)

  println(GeneticAlgorithm.loss(samples)(wgd0, wgd1))
  println(GeneticAlgorithm.loss(samples)(wga0, wga1))

  println(wgd0 + " " + wgd1)
  println(wga0 + " " + wga1)

  val areaFigure = Figure()
  val areaPlot = areaFigure.subplot(0)
  areaPlot += scatter(samples.map(_.house.area), samples.map(_.price.toDouble), _ => 0.01, colors = _ => Color.red)
  areaPlot.title = "Area"

  val x = linspace(0, 1.0)
  areaPlot += plot(x, x.map(_ * wgd0), '.')
  areaPlot += plot(x, x.map(_ * wga0), '.')

  val roomsFigure = Figure()
  val roomsPlot = roomsFigure.subplot(0)
  roomsPlot += scatter(samples.map(_.house.rooms), samples.map(_.price.toDouble), _ => 0.01, colors = _ => Color.red)
  roomsPlot.title = "Rooms"

  val y = linspace(0, 1.0)
  roomsPlot += plot(y, y.map(_ * wgd1), '.')
  roomsPlot += plot(y, y.map(_ * wga1), '.')

  val a = LinearRegression(wga0 / maxArea * maxPrice, wga1 / maxRooms * maxPrice)
  while (true) {
    val area = StdIn.readInt()
    val rooms = StdIn.readInt()
    println(a(House(area, rooms)))
  }
}
