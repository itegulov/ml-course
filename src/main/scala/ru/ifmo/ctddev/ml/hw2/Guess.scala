package ru.ifmo.ctddev.ml.hw2

import scala.io.StdIn

object Guess extends App {
  val samples = HouseWithPrice.parseData(getClass.getResourceAsStream("/prices.txt"))
  val (w0, w1) = GeneticAlgorithm(samples, 0, 0)
  val a = LinearRegression(w0, w1)
  while (true) {
    val area = StdIn.readInt()
    val rooms = StdIn.readInt()
    println(a(House(area, rooms)))
  }
}
