package ru.ifmo.ctddev.ml.hw2

/**
  * Created by Алексей on 02.10.2016.
  */
object Utils {

  def mseLoss(data: Seq[Data])(coefficients: Seq[Double]): Double = {
    val sum = data.map {
      case Data(features, answer) =>
        val predictedAnswer = coefficients.zip(features).map { case (f, s) => f * s }.sum
        Math.pow(answer - predictedAnswer, 2)
    }.sum

    Math.pow(sum / data.length, 0.5)
  }

  def mseLossDerivative(data: Seq[Data])(coefficients: Seq[Double]): Seq[Double] = {
    for (i <- coefficients.indices) yield {
      val denom = data.map {
        case Data(features, answer) =>
          val predictedAnswer = coefficients.zip(features).map { case (f, s) => f * s }.sum
          -(answer - predictedAnswer) * features(i)
      }.sum
      val nomin = data.map {
        case Data(features, answer) =>
          val predictedAnswer = coefficients.zip(features).map { case (f, s) => f * s }.sum
          Math.pow(answer - predictedAnswer, 2)
      }.sum
      denom / nomin / Math.sqrt(data.size)
    }
  }
}
