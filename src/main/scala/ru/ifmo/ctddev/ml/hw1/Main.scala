package ru.ifmo.ctddev.ml.hw1

import java.awt.Color
import java.io.File

import Math.abs

import breeze.plot._

object Main {

  def normalize(initSamples: Seq[PointWithClass]): Seq[PointWithClass] = {
    val maxX = abs(initSamples.maxBy(a => abs(a.point.x)).point.x)
    val maxY = abs(initSamples.maxBy(a => abs(a.point.y)).point.y)
    initSamples.map {
      case PointWithClass(Point(x, y), pClass) =>
        PointWithClass(Point(x / maxX, y / maxY), pClass)
    }
  }

  def polarize(points: Seq[PointWithClass]): Seq[PointWithClass] = {
    for (PointWithClass(Point(x, y), pClass) <- points) yield {
      val r = Math.sqrt(x * x + y * y)
      val phi = Math.atan2(y, x)
      PointWithClass(Point(r, phi), pClass)
    }
  }

  def drawPoints(zeros: Seq[PointWithClass], ones: Seq[PointWithClass]): Unit = {
    val pointsFigure = Figure()
    val pointsPlot = pointsFigure.subplot(0)
    pointsPlot += scatter(zeros.map(_.point.x), zeros.map(_.point.y), _ => 0.02, colors = _ => Color.red)
    pointsPlot += scatter(ones.map(_.point.x), ones.map(_.point.y), _ => 0.02, colors = _ => Color.blue)
    pointsPlot.title = "Data points"
  }

  def drawAccuracyPlot(accuracies: Seq[(Int, Double)]): Unit = {
    val (ks, accs) = accuracies.unzip
    val accuraciesFigure = Figure()
    val accuracyPlot = accuraciesFigure.subplot(0)
    accuracyPlot += plot(ks.map(_.toDouble), accs)
    accuracyPlot.title = "Accuracies"
  }

  def calculateAccuracy(test: Seq[PointWithClass], algorithm: Point => PointClass): Double = {
    test.map {
      case PointWithClass(point, pointClass) =>
        if (algorithm(point) != pointClass) 0 else 1
    }.sum.toDouble / test.size
  }

  val kernels = Seq[(String, Double => Double)](
    ("Triangle", x => 1 - x),
    ("Uniform", x => 0.5),
    ("Quartic", x => (1 - x * x) * 0.75),
    ("Cosine", x => Math.cos(x * Math.PI / 2))
  )

  val metrics = Seq[(String, (Point, Point) => Double)](
    ("Euclid", Distances.euclidean)
  )

  val weightDistances = Seq[(String, (Point, Point) => Double)](
    ("Euclid", Distances.euclidean),
    ("Manhattan", Distances.manhattan)
  )

  val foldNumber = 5

  def main(args: Array[String]): Unit = {
    for ((kernelName, kernel)        <- kernels;
         (metricName, metric)        <- metrics;
         (weightName, weightDistance) <- weightDistances) {
      println(s"kernel: $kernelName    metric: $metricName    weightDistance: $weightName")
      val maxDistance = metric(Point(-1, -1), Point(1, 1))

      val weight = Distances.kernelize((f, s) => weightDistance(f, s) / maxDistance, kernel)
      val initSamples = PointWithClass.parseData(new File(getClass.getResource("/chips.txt").toURI))
      val samples = normalize(polarize(initSamples))
      val (zeros, ones) = samples.partition(_.pointClass == Zero)

//      drawPoints(zeros, ones)

      val folds = KFoldCrossValidation(samples, samples.length)
      val accuracies = for (k <- 1 to samples.length) yield {
        val results = folds.map {
          case (train, test) =>
            calculateAccuracy(test, KNNMethod.train(train, k, metric, weight))
        }
        k -> results.sum / results.length
      }

//      drawAccuracyPlot(accuracies)

      val bestK = accuracies.maxBy(_._2)._1
      println(s"Best k is $bestK")

      val kFolds = KFoldCrossValidation(samples, foldNumber)
      val result = for ((train, test) <- kFolds) yield {
        val algorithm = KNNMethod.train(train, bestK, metric, weight)
        calculateAccuracy(test, algorithm)
      }
      val answer = result.sum / result.length
      println(s"Accuracy of algorithm is: $answer")
    }
  }
}
