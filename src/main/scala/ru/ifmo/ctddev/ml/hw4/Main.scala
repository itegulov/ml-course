package ru.ifmo.ctddev.ml.hw4

import java.awt.Color
import java.io.File

import breeze.plot._
import ru.ifmo.ctddev.ml.Metric
import ru.ifmo.ctddev.ml.hw1._
import ru.ifmo.ctddev.ml.hw1

object Main {

  def drawPoints(zeros: Seq[PointWithClass], ones: Seq[PointWithClass]): Unit = {
    val pointsFigure = Figure()
    val pointsPlot = pointsFigure.subplot(0)
    pointsPlot += scatter(zeros.map(_.point.x), zeros.map(_.point.y), _ => 0.02, colors = _ => Color.red)
    pointsPlot += scatter(ones.map(_.point.x), ones.map(_.point.y), _ => 0.02, colors = _ => Color.blue)
    pointsPlot.title = "Data points"
  }

  def toData(points: Seq[PointWithClass]): Seq[Data] = {
    for (PointWithClass(Point(x, y), c) <- points) yield {
      val classValue = c match {
        case One => 1
        case Zero => -1
      }
      Data(Seq(x, y), classValue)
    }
  }

  def addPolarFeatures(points: Seq[Data]): Seq[Data] = {
    for (Data(Seq(x, y), c) <- points) yield {
      //      Data(Seq(x, y), classValue)
      Data(Seq(x, y, x * x, y * y, x * y), c)
//      Data(Seq(x, y, x * x, y * y, x * y, x * x * x, x * x * y, x * y * y, y * y * y), classValue)
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

  def wilcoxon(x1: Seq[Double], x2: Seq[Double]): Double = {
    val abses = x1.zip(x2).map { case (a, b) => Math.abs(a - b) }
    val signes = x1.zip(x2).map { case (a, b) => if (a - b >= 0) 1 else -1 }
    val reduced = abses.zip(signes).filter { case (a, _) => a != 0 }
    val sortedReduced = reduced.sortBy(_._1)
    val w = sortedReduced.zipWithIndex.map { case ((_, sign), r) => sign * (r + 1) }.sum
    w
  }

  val foldNumber = 5

  def main(args: Array[String]): Unit = {
    val pts = PointWithClass.parseData(new File(getClass.getResource("/chips.txt").toURI))
    val samples = normalize(addPolarFeatures(normalize(toData(pts)))).toIndexedSeq
    val pts2 = samples.map { case Data(features, answer) => PointWithClass(Point(features(0), features(1)), if (answer == 1) One else Zero) }
    val (zeros, ones) = pts2.partition(_.pointClass == Zero)

    drawPoints(zeros, ones)

    val folds = KFoldCrossValidation(samples, foldNumber)

    val realResults = folds.map {
      case (_, test) =>
        test.map(_.answer)
    }.map(toBoolean)
    val results = folds.map {
      case (train, test) =>
        val algorithm = new SVM(train).train
        test.map(_.features).map(algorithm)
    }.map(toBoolean)
    val f1Score = Metric.f1Score(realResults.flatten, results.flatten)
    val knnPts = hw1.Main.normalize(hw1.Main.polarize(pts))
    val kNNfolds = hw1.KFoldCrossValidation(knnPts, foldNumber)
    val kNNRealResults = kNNfolds.map {
      case (_, test) =>
        test.map(_.pointClass).map(x => if (x == Zero) 0 else 1)
    }
    val kNNResults = kNNfolds.map {
      case (train, test) =>
        val algorithm = KNNMethod.train(train, 13, Distances.euclidean, Distances.euclidean)
        test.map(_.point).map(algorithm).map(x => if (x == Zero) 0 else 1)
    }
    val kNNf1Score = Metric.f1Score(kNNRealResults.flatten, kNNResults.flatten)
    println(s"f1 score: $f1Score")
    println(s"f1 score: $kNNf1Score")
    val foldF1 = results.zip(realResults).map {
      case (x, y) => Metric.f1Score(x, y) * 100
    }
    val kNNFoldF1 = kNNResults.zip(kNNRealResults).map {
      case (x, y) => Metric.f1Score(x, y) * 100
    }
    val sigma = foldF1.zip(kNNFoldF1).map {
      case (x, y) => (x - y) * (x - y) / x
    }.sum
    println(s"Chi square: $sigma")
    println(s"Wilcoxon: ${wilcoxon(foldF1, kNNFoldF1)}")
  }
}
