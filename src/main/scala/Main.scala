import java.awt.Color
import java.io.File

import breeze.plot._

object Main extends App {
  val samples = PointWithClass.parseData(new File(getClass.getResource("chips.txt").toURI))
  val (ones, zeros) = samples.partition(_.pointClass == One)

  val pointsFigure = Figure()
  val pointsPlot = pointsFigure.subplot(0)
  pointsPlot += scatter(zeros.map(_.point.x), zeros.map(_.point.y), _ => 0.02, colors = _ => Color.red)
  pointsPlot += scatter(ones.map(_.point.x), ones.map(_.point.y), _ => 0.02, colors = _ => Color.blue)
  pointsPlot.title = "Data points"

  val folds = KFoldCrossValidation(samples, samples.length)
  val accuracies = for (k <- 1 to 20) yield {
    val results = folds.map {
      case (train, test) =>
        val resultMethod = KNNMethod.train(train, k, Distances.euclidean, Weights.euclidean)
        val hits = test.map {
          case PointWithClass(point, pointClass) =>
            if (resultMethod(point) != pointClass) 0 else 1
        }.sum
        hits.toDouble / test.size
    }
    val accuracy = results.sum / results.length
    println(s"$k -> $accuracy")
    accuracy
  }

  val accuraciesFigure = Figure()
  val accuracyPlot = accuraciesFigure.subplot(0)
  accuracyPlot += plot(1 to accuracies.length map(_.toDouble), accuracies)
  accuracyPlot.title = "Accuracies"
}
