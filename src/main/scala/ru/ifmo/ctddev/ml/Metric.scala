package ru.ifmo.ctddev.ml

object Metric {

  private def fScore(beta: Int)(predictions: Seq[Int], correct: Seq[Int]): Double = {
    val zip = predictions.zip(correct)
    val truePositives = zip.collect {
      case x @ (1, 1) => x
    }.length
    val falseNegative = zip.collect {
      case x @ (1, 0) => x
    }.length
    val precision = truePositives * 1d / (truePositives + falseNegative)
    val recall = truePositives * 1d / correct.count(_ == 1)
    (1 + beta * beta) * (precision * recall) / (beta * beta * precision + recall)
  }

  val f1Score: (Seq[Int], Seq[Int]) => Double = fScore(1)
}
