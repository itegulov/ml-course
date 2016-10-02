package ru.ifmo.ctddev.ml.hw2

import java.awt.Color

import breeze.numerics._
import breeze.linalg._
import breeze.plot._

import scala.io.StdIn

object Main {

  def main(args: Array[String]): Unit = {
    val houses = HouseWithPrice.parseHouses(getClass.getResourceAsStream("/prices.txt"))
    val (data, means, sigmas) = Data.normalize(houses.map(_.toData))
    val geneticCoefficients = GeneticAlgorithm.fit(Seq(0, 0), Utils.mseLoss(data))
    println(s"Coefficients from genetic algorithm: ${geneticCoefficients.mkString(", ")}")
    val geneticUnnormalizedCoefficients = Data.unnormalize(geneticCoefficients, means, sigmas)
    println(s"Unnormalized coefficients from genetic algorithm: ${geneticUnnormalizedCoefficients.mkString(", ")}")
    val geneticPredictor = LinearRegression(geneticUnnormalizedCoefficients)

    val gradientCoefficients = GradientDescent.fit(Seq(0, 0), Utils.mseLoss(data))
    println(s"Coefficients from gradient algorithm: ${gradientCoefficients.mkString(", ")}")
    val gradientPredictor = LinearRegression(Data.unnormalize(geneticCoefficients, means, sigmas))

    val areaFigure = Figure()
    val areaPlot = areaFigure.subplot(0)
    areaPlot += scatter(houses.map(_.house.area), houses.map(_.price), _ => 20.0, colors = _ => Color.red)
    areaPlot.title = "Area"

    val l = linspace(0, 5000.0)
    areaPlot += plot(l, l.map(_ * geneticCoefficients.head), '.')
    areaPlot += plot(l, l.map(_ * gradientCoefficients.head), '.')

    while (true) {
      val area = StdIn.readInt()
      val rooms = StdIn.readInt()
      println("Genetic-predicted price: " + geneticPredictor(Seq(area, rooms)))
//      println("Gradient-predicted price: " + gradientPredictor(Seq(area, rooms)))
    }
  }
}
