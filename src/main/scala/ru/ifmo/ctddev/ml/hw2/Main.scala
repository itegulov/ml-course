package ru.ifmo.ctddev.ml.hw2

import java.awt.Color

import breeze.numerics._
import breeze.linalg._
import breeze.plot._

import scala.io.StdIn

object Main {

  def main(args: Array[String]): Unit = {
    val houses = HouseWithPrice.parseHouses(getClass.getResourceAsStream("/prices.txt"))
    val unnormalizedData = houses.map(_.toData)
    val (data, means, sigmas) = Data.normalize(unnormalizedData)
    val coolData = unnormalizedData.map {
      case Data(features, answer) =>
        Data(features :+ 1.0, answer)
    }
    val geneticCoefficients = GeneticAlgorithm.fit(Seq(0, 0), Utils.mseLoss(data))
    println(s"Coefficients from genetic algorithm: ${geneticCoefficients.mkString(", ")}")
    val geneticUnnormalizedCoefficients = Data.unnormalize(geneticCoefficients, means, sigmas)
    println(s"Unnormalized coefficients from genetic algorithm: ${geneticUnnormalizedCoefficients.mkString(", ")}")
    val geneticPredictor = LinearRegression(geneticUnnormalizedCoefficients)
    println(s"MSE for genetic is ${Utils.mseLoss(coolData)(geneticUnnormalizedCoefficients)}")

    val gradientCoefficients = GradientDescent.fit(Seq(0, 0), Utils.mseLossDerivative(data))
    println(s"Coefficients from gradient algorithm: ${gradientCoefficients.mkString(", ")}")
    val gradientUnnormalizedCoefficients = Data.unnormalize(gradientCoefficients, means, sigmas)
    println(s"Unnormalized coefficients from gradient algorithm: ${gradientUnnormalizedCoefficients.mkString(", ")}")
    val gradientPredictor = LinearRegression(gradientUnnormalizedCoefficients)
    println(s"MSE for gradient is ${Utils.mseLoss(coolData)(gradientUnnormalizedCoefficients)}")

    val unnormalizedDebug = Data.unnormalize(Seq(1.02, 0.02), means, sigmas)
    println(Utils.mseLoss(coolData)(unnormalizedDebug))

    val areaFigure = Figure()
    val areaPlot = areaFigure.subplot(0)
    areaPlot += scatter(houses.map(_.house.area), houses.map(_.price), _ => 20.0, colors = _ => Color.red)
    areaPlot.title = "Area"

    val areaLinspace = linspace(0, 5000.0)
    areaPlot += plot(areaLinspace, areaLinspace.map(v => geneticPredictor(Seq(v, 0, 1))), '.')
    areaPlot += plot(areaLinspace, areaLinspace.map(v => gradientPredictor(Seq(v, 0, 1))), '.')

    val normFigure = Figure()
    val normPlot = normFigure.subplot(0)
    normPlot += scatter(data.map(_.features.head), data.map(_.answer), _ => 0.01, colors = _ => Color.red)

    val unitLinspace = linspace(-1.0, 1.0)
    normPlot += plot(unitLinspace, unitLinspace.map(_ * geneticCoefficients.head), '.')
    normPlot += plot(unitLinspace, unitLinspace.map(_ * gradientCoefficients.head), '.')

    while (true) {
      val area = StdIn.readInt()
      val rooms = StdIn.readInt()
      println("Genetic-predicted price: " + geneticPredictor(Seq(area, rooms, 1)))
      println("Gradient-predicted price: " + gradientPredictor(Seq(area, rooms, 1)))
    }
  }
}
