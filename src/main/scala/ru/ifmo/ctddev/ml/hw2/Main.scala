package ru.ifmo.ctddev.ml.hw2

object Main extends App {
  val samples = HouseWithPrice.parseData(getClass.getResourceAsStream("/prices.txt"))
//  val (w0, w1) = GeneticAlgorithm(samples, 0, 0)
  val (w0, w1) = GradientDescent.gradientDescent(samples, 0, 0)
  println(GeneticAlgorithm.loss(samples)(w0, w1))
}
