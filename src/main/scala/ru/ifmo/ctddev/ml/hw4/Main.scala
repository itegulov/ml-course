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

  def addPolarFeatures(points: Seq[PointWithClass]): Seq[Data] = {
    for (PointWithClass(Point(x, y), c) <- points) yield {
      val classValue = c match {
        case One => 1
        case Zero => -1
      }
      Data(Seq(x, y, x * x, y * y), classValue)
    }
  }

  def toBoolean(intSeq: Seq[Int]): Seq[Int] = {
    intSeq.map {
      case 1 => 1
      case -1 => 0
      case _ => throw new IllegalArgumentException("Int seq should contains only 1 and -1")
    }
  }

  val foldNumber = 5

  def main(args: Array[String]): Unit = {
    val samples = addPolarFeatures(PointWithClass.parseData(new File(getClass.getResource("/chips.txt").toURI)))
    val (zeros, ones) = samples.partition(_.answer == -1)

//    drawPoints(zeros, ones)

    val folds = KFoldCrossValidation(samples, samples.length)

    val realResults = toBoolean(samples.map(_.answer))
    val results = toBoolean(folds.flatMap {
      case (train, test) =>
        val algorithm = SVM.train(train)
        test.map(_.features).map(algorithm)
    })
    val f1Score = Metric.f1Score(realResults, results)
    println(s"f1 score: $f1Score")
  }
}
