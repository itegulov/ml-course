package ru.ifmo.ctddev.ml.hw2

import scala.util.Random

object GeneticAlgorithm extends LinearFitter {
  override def fit(initialCoefficients: Seq[Double], loss: Seq[Double] => Double): Seq[Double] = {
    var brood = Seq(initialCoefficients)
    for (i <- 1 to 50) {
      val newBrood = for (
        individual <- brood;
        times <- 1 to 6
      ) yield individual.map(_ + (Random.nextDouble() * 2 - 1))

      brood = newBrood.sortBy(loss).take(50)
    }
    brood.minBy(loss)
  }
}
