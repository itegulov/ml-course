package ru.ifmo.ctddev.ml.hw1

import java.awt.Color
import java.io.File

import breeze.plot._

/**
  * Created by Aleksei Latyshev on 26.09.2016.
  */
object Test extends App {
  val samples = PointWithClass.parseData(new File(getClass.getResource("chips.txt").toURI))
  val (ones, zeros) = samples.partition(_.pointClass == One)

  val pointsFigure = Figure()
  val pointsPlot = pointsFigure.subplot(0)
  pointsPlot += scatter(zeros.map(_.point.x), zeros.map(_.point.y), _ => 0.02, colors = _ => Color.red)
  pointsPlot += scatter(ones.map(_.point.x), ones.map(_.point.y), _ => 0.02, colors = _ => Color.blue)
  pointsPlot.title = "Data points"

  val folds = KFoldCrossValidation(samples, 5)
  for ((train, test) <- folds) {
    val a = KNNMethod.train(train, 9, Distances.kernelize(Distances.generalizedEuclidean(1, 1.5), Math.exp), Weights.euclidean)
    val correct = test.map {
      case PointWithClass(point, pointClass) => if (a(point) == pointClass) 1 else 0
    }.sum
    println(correct.toDouble / test.size)
  }

}
