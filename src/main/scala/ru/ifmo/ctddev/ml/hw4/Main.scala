package ru.ifmo.ctddev.ml.hw4

import java.awt.Color
import java.io.File

import breeze.plot._
import ru.ifmo.ctddev.ml.Metric
import ru.ifmo.ctddev.ml.hw1._

object Main {

  def drawPoints(zeros: Seq[PointWithClass], ones: Seq[PointWithClass]): Unit = {
    val pointsFigure = Figure()
    val pointsPlot = pointsFigure.subplot(0)
    pointsPlot += scatter(zeros.map(_.point.x), zeros.map(_.point.y), _ => 0.02, colors = _ => Color.red)
    pointsPlot += scatter(ones.map(_.point.x), ones.map(_.point.y), _ => 0.02, colors = _ => Color.blue)
    pointsPlot.title = "Data points"
  }

  val foldNumber = 5

  def main(args: Array[String]): Unit = {
    val samples = PointWithClass.parseData(new File(getClass.getResource("/chips.txt").toURI))
    val (zeros, ones) = samples.partition(_.pointClass == Zero)

    drawPoints(zeros, ones)

    val folds = KFoldCrossValidation(samples, samples.length)

    val realResults = samples.map(_.pointClass).map {
      case One => 1
      case Zero => 0
    }
    val results = folds.flatMap {
      case (train, test) =>
        val algorithm = SVM.train(train)
        test.map(_.point).map(algorithm)
    }.map {
      case One => 1
      case Zero => 0
    }
    val f1Score = Metric.f1Score(realResults, results)
    println(s"f1 score: $f1Score")
  }
}
