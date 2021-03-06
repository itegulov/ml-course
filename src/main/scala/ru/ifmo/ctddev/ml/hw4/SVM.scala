package ru.ifmo.ctddev.ml.hw4

import scala.concurrent.{ Future, Await }

class SVM(trainSet: Seq[Data]) {
  val eps = 1e-3
  val C = 100

  val n = trainSet.head.features.size
  val l = trainSet.size
  var alpha = Seq.fill(l)(0D)

  def findCurrentAlgorithm: Seq[Double] => Double = {
    val w = (0 until n).map(i =>
      (0 until l).map(j =>
        alpha(j) * trainSet(j).answer * trainSet(j).features(i)
      ).sum
    )
    val (_, cInd) = alpha.zipWithIndex.maxBy(_._1)
    val w0 = crossProduct(w, trainSet(cInd).features) - trainSet(cInd).answer
    testData =>
      (0 until l).map(i =>
        alpha(i) * trainSet(i).answer * crossProduct(testData, trainSet(i).features)
      ).sum - w0
  }

  //p == y, q == x

  def getQ(i: Int, j: Int, ai: Double, aj: Double, p: Int, q: Int): QuadraticForm = {
    i match {
      case _ if i == p => j match {
        case _ if j == q => QuadraticForm(0, 0, 1, 0, 0, 0)
        case _ if j == p => QuadraticForm(0, 0, 0, 0, 1, 0)
        case _ => QuadraticForm(0, 0, 0, aj, 0, 0)
      }
      case _ if i == q => j match {
        case _ if j == q => QuadraticForm(1, 0, 0, 0, 0, 0)
        case _ if j == p => QuadraticForm(0, 0, 1, 0, 0, 0)
        case _ => QuadraticForm(0, aj, 0, 0, 0, 0)
      }
      case _ => j match {
        case _ if j == q => QuadraticForm(0, ai, 0, 0, 0, 0)
        case _ if j == p => QuadraticForm(0, 0, 0, ai, 0, 0)
        case _ => QuadraticForm(0, 0, 0, 0, 0, ai * aj)
      }
    }
  }

  def getQ(i: Int, ai: Double, p: Int, q: Int): QuadraticForm = {
    i match {
      case _ if i == p => QuadraticForm(0, 0, 0, 1, 0, 0)
      case _ if i == q => QuadraticForm(0, 1, 0, 0, 0, 0)
      case _ => QuadraticForm(0, 0, 0, 0, 0, ai)
    }
  }

  val cached = for {
    i <- trainSet.indices
  } yield for {
    j <- trainSet.indices
  } yield {
    crossProduct(trainSet(i).features, trainSet(j).features) *
      trainSet(i).answer *
      trainSet(j).answer
  }

  def findMax(p: Int, q: Int): Int = {
    if (p == q) return 0
    val quad = trainSet.indices.zip(alpha).map {
      case (i, a) => getQ(i, a, p, q)
    }.reduce(_ + _)

    val minus = for {
      i <- trainSet.indices
      j <- trainSet.indices
    } yield {
      getQ(i, j, alpha(i), alpha(j), p, q) * cached(i)(j)
    }
    val k = -trainSet(p).answer * trainSet(q).answer
    val b = -trainSet(p).answer * trainSet.indices
      .filter(i => i != p && i != q)
      .map(i => alpha(i) * trainSet(i).answer)
      .sum
    val qq = (quad - minus.reduce(_ + _) * 0.5).substituteY(k, b)
    val xv = -qq.xC / (2 * qq.xxC)
    val L = if (k > 0) Math.max(0, -b / k) else Math.max(0, (C - b) / k)
    val H = if (k > 0) Math.min(C, (C - b) / k) else Math.min(C, -b / k)
    val x = if (xv > H) H else if (xv < L) L else xv
    val y = k * x + b
    if (Math.abs(y - alpha(p)) > eps * (y + alpha(p) + eps)) {
      println(s"New loss: ${
        qq.calc(x, 0)
      }")
      alpha = alpha.updated(p, y).updated(q, x)
      1
    } else {
      0
    }
  }

  def findMax(q: Int): Int = {
    val currentAlgo = findCurrentAlgorithm
    val qE = currentAlgo(trainSet(q).features) - trainSet(q).answer
    val qR = qE * trainSet(q).answer
    if ((qR < -eps && alpha(q) < C - eps) || (qR > eps && alpha(q) > eps)) {
      if (alpha.count(a => a > eps && a < C - eps) > 1) {
        val (_, p) = alpha.zipWithIndex
          .filter {
            case (a, _) => a > eps && a < C - eps
          }
          .minBy {
            case (a, i) =>
              val pE = currentAlgo(trainSet(i).features) - trainSet(i).answer
              Math.abs(pE - qE)
          }
        if (findMax(p, q) > 0) {
          return 1
        }
      }
      for (i <- 0 until l) if (alpha(i) > eps && alpha(i) < C - eps) {
        if (findMax(i, q) > 0) {
          return 1
        }
      }
      for (i <- 0 until l) {
        if (findMax(i, q) > 0) {
          return 1
        }
      }
    }
    0
  }

  def crossProduct(x: Seq[Double], y: Seq[Double]): Double = {
    x.zip(y).map {
      case (l, r) => l * r
    }.sum
  }

  def train: Seq[Double] => Int = {
    println(s"kek")

    var examineAll = true
    var numChanged = 0
    var iter = 0

    while ((numChanged > 0 || examineAll) && iter < 400) {
      numChanged = 0
      if (examineAll) {
        for (i <- 0 until l) {
          numChanged += findMax(i)
        }
      } else {
        for (i <- 0 until l) if (alpha(i) > eps && alpha(i) < C - eps) {
          numChanged += findMax(i)
        }
      }
      if (examineAll) examineAll = false
      else if (numChanged == 0) examineAll = true
      iter += 1
    }

    val w = (0 until n).map(i =>
      (0 until l).map(j =>
        alpha(j) * trainSet(j).answer * trainSet(j).features(i)
      ).sum
    )
    val (_, cInd) = alpha.zipWithIndex.maxBy(_._1)
    val w0 = crossProduct(w, trainSet(cInd).features) - trainSet(cInd).answer

    if (n == 9) {
      printf("%.2f * x + %.2f * y + %.2f * x^2 + %.2f * x * y + %.2f * y^2 + %.2f * x^3 + %.2f * x^2 * y + %.2f * x * y^2 + %.2f * y^3 = %.2f\n", w(0), w(1), w(2), w(3), w(4), w(5), w(6), w(7), w(8), w0)
    } else if (n == 5) {
      printf("%.2f * x + %.2f * y + %.2f * x^2 + %.2f * x * y + %.2f * y^2 = %.2f\n", w(0), w(1), w(2), w(3), w(4), w0)
    }
    testData => {
      val res: Double = (0 until l).map(i =>
        alpha(i) * trainSet(i).answer * crossProduct(testData, trainSet(i).features)
      ).sum - w0

      if (res >= 0) 1 else -1
    }
  }
}
