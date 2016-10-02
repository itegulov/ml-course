package ru.ifmo.ctddev.ml.hw2

import java.awt.Color

import breeze.numerics._
import breeze.linalg._
import breeze.plot._

import scala.io.StdIn

object Main {

  def main(args: Array[String]): Unit = {
    val houses = HouseWithPrice.parseHouses(getClass.getResourceAsStream("/prices.txt"))
    val data = Data.normalize(houses.map(_.toData))
    val geneticCoefficients = GeneticAlgorithm.fit(Seq(0, 0), Utils.mseLoss(data))
    println(s"Coefficients from genetic algorithm: ${geneticCoefficients.mkString(", ")}")
    val geneticPredictor = LinearRegression(geneticCoefficients)
    while (true) {
      val area = StdIn.readInt()
      val rooms = StdIn.readInt()
      println("Genetic-predicted price: " + geneticPredictor(Seq(area, rooms)))
    }
  }
}
