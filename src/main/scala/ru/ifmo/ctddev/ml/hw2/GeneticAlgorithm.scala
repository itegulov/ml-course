package ru.ifmo.ctddev.ml.hw2

import scala.util.Random

object GeneticAlgorithm {
  def loss(houses: Seq[HouseWithPrice])(w0: Double, w1: Double): Double = {
    houses.map {
      case HouseWithPrice(House(area, rooms), price) =>
        val predictedPrice = w0 * area + w1 * rooms
        Math.pow(price - predictedPrice, 2)
    }.sum
  }

  def apply(houses: Seq[HouseWithPrice], w0: Double, w1: Double): (Double, Double) = {
    var brood = Seq((w0, w1))
    for (i <- 1 to 10) {
      val crossBrood = for (
        (w0f, w1f) <- brood;
        (w0s, w1s) <- brood
      ) yield (w0f + Random.nextDouble * (w0s - w0f), w1f + Random.nextDouble * (w1s - w1f))

      val mutations = for ((w0, w1) <- brood) yield
        (w0 + 100 * Random.nextDouble(), w1 + 100000 * Random.nextDouble())

      val newBrood = crossBrood ++ mutations
      brood = newBrood.sortBy {
        case (f, s) => loss(houses)(f, s)
      }.take(100)
    }
    brood.sortBy {
      case (f, s) => loss(houses)(f, s)
    }.head
  }
}
