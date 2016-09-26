package ru.ifmo.ctddev.ml.hw2

import scala.util.Random

object GeneticAlgorithm {
  def loss(houses: Seq[NormalizedHouseWithPrice])(w0: Double, w1: Double): Double = {
    val sum = houses.map {
      case NormalizedHouseWithPrice(NormalizedHouse(area, rooms), price) =>
        val predictedPrice = w0 * area + w1 * rooms
        Math.pow(price - predictedPrice, 2)
    }.sum

    Math.pow(sum, 0.5) / houses.length
  }

  def apply(houses: Seq[NormalizedHouseWithPrice], w0: Double, w1: Double): (Double, Double) = {
    val thisLoss: ((Double, Double) => Double) = loss(houses)
    var brood = Seq((w0, w1))
    for (i <- 1 to 20) {
      val crossBrood = for (
        (w0f, w1f) <- brood;
        (w0s, w1s) <- brood;
        coefficient = Random.nextDouble
      ) yield (w0f + coefficient * (w0s - w0f), w1f + coefficient * (w1s - w1f))

      val mutations = for ((w0, w1) <- brood; times <- 1 to 10) yield
        (w0 + (Random.nextDouble() - 0.5), w1 + (Random.nextDouble() - 0.5))

      val newBrood = crossBrood ++ mutations
      val temp = newBrood.map(thisLoss.tupled)
      brood = newBrood.sortBy(thisLoss.tupled).take(50)
      println(brood.map(thisLoss.tupled))
    }
    brood.minBy {
      case (f, s) => loss(houses)(f, s)
    }
  }
}
