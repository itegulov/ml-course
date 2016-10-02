package ru.ifmo.ctddev.ml.hw2

case class Data(features: Seq[Double], answer: Double)

object Data {
  def normalize(list: Seq[Data]): (Seq[Data], Seq[Double], Seq[Double]) = {
    def rec(lis: Seq[Seq[Double]]): (Seq[Seq[Double]], Seq[Double], Seq[Double]) = lis match {
      case Seq(Seq(), _*) =>
        (Seq.empty, Seq.empty, Seq.empty)
      case _ =>
        val (leftNorm, leftMean, leftSigma) = rec(lis.map(_.tail))
        val (myNorm, myMean, mySigma) = normalizeDoubles(lis.map(_.head))
        (myNorm +: leftNorm, myMean +: leftMean, mySigma +: leftSigma)
    }
    val (normAnswers, meanAnswers, sigmaAnswers) = normalizeDoubles(list.map(_.answer))
    val (normFeatures, meanFeatures, sigmaFeatures) = rec(list.map(_.features))
    val normObjects = normFeatures.transpose
    val normalized = normObjects.zip(normAnswers).map {
      case (list1, list2) => Data(list1, list2)
    }
    (normalized, meanFeatures :+ meanAnswers, sigmaFeatures :+ sigmaAnswers)
  }

  def normalizeDoubles(list: Seq[Double]): (Seq[Double], Double, Double) = {
    val mean = list.sum / list.size
    val sigma = Math.sqrt(list.map(x => (x - mean) * (x - mean)).sum / (list.size - 1))
    (list.map(x => (x - mean) / sigma), mean, sigma)
  }

  def unnormalize(coefficients: Seq[Double], mean: Seq[Double], sigma: Seq[Double]): Seq[Double] = {
    val sigmaZ = sigma.last
    val meanZ = mean.last
    coefficients.zip(sigma).map{
      case (x, y) => x / y * meanZ
    } :+ (meanZ - mean.zip(sigma).zip(coefficients).map({
      case ((x, y), z) => x * z / y
    }).sum)
  }
}
