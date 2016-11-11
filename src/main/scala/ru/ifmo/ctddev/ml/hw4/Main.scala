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
      //      Data(Seq(x, y), classValue)
      Data(Seq(x, y, x * x, y * y, x * y), classValue)
    }
  }

  def toBoolean(intSeq: Seq[Int]): Seq[Int] = {
    intSeq.map {
      case 1 => 1
      case -1 => 0
      case _ => throw new IllegalArgumentException("Int seq should contains only 1 and -1")
    }
  }

  def normalize(seq: Seq[Data]): Seq[Data] = {
    val mean = seq.head.features.indices.map(i =>
      seq.map(_.features(i)).sum / seq.size
    )
    val maximum = seq.head.features.indices.map(i =>
      seq.map(_.features(i)).sorted.head
    )
    for (Data(features, answer) <- seq) yield {
      Data(features.zipWithIndex.map { case (feature, index) => (feature - mean(index)) / (maximum(index) - mean(index)) }, answer)
    }
  }

  val foldNumber = 5

  def main(args: Array[String]): Unit = {
    val pts = PointWithClass.parseData(new File(getClass.getResource("/chips.txt").toURI))
    val samples = addPolarFeatures(pts).toIndexedSeq
    val (zeros, ones) = pts.partition(_.pointClass == Zero)

//    drawPoints(zeros, ones)

    val folds = KFoldCrossValidation(samples, foldNumber)

    val realResults = toBoolean(folds.flatMap {
      case (_, test) =>
        test.map(_.answer)
    })
    val results = toBoolean(folds.flatMap {
      case (train, test) =>
        val algorithm = new SVM(train).train
        test.map(_.features).map(algorithm)
    })
    val f1Score = Metric.f1Score(realResults, results)
    println(s"f1 score: $f1Score")
  }
}
