package ru.ifmo.ctddev.ml.hw4

import scala.util.Random

class SVM(trainSet: Seq[Data]) {

  val C = 100

  val rnd = new Random(3487473837L)

  //p == y, q == x

  def getQ(i: Int, j: Int, ai : Double, aj : Double, p: Int, q: Int): QuadraticForm = {
    if ((j == p || j == q) && i != p && i != q) {
      getQ(j, i, aj, ai, p, q)
    } else {
      if (i != p && i != q) {
        QuadraticForm(0, 0, 0, 0, 0, ai * aj)
      } else if (i == q) {
        j match {
          case _ if j == q => QuadraticForm(1, 0, 0, 0, 0, 0)
          case _ if j == p => QuadraticForm(0, 0, 1, 0, 0, 0)
          case _ => QuadraticForm(0, aj, 0, 0, 0, 0)
        }
      } else {
        j match {
          case _ if j == q => QuadraticForm(0, 0, 1, 0, 0, 0)
          case _ if j == p => QuadraticForm(0, 0, 0, 0, 1, 0)
          case _ => QuadraticForm(0, 0, 0, aj, 0, 0)
        }
      }
    }
  }

  def getQ(i: Int, ai : Double, p: Int, q: Int): QuadraticForm = {
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

  def findMax(p: Int, q: Int, alpha: Seq[Double], C : Double): (Double, Seq[Double]) = {
    val quad = trainSet.indices.zip(alpha).map { case (i, a) => getQ(i, a, p, q)}.reduce(_ + _)
    val minus = for { i <- trainSet.indices
          j <- trainSet.indices } yield {
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
    def qValue(xx: Double) = xx * xx * qq.xxC + xx * qq.xC + qq.C
    val xvValue = qValue(x)
    val lValue = qValue(L)
    val hValue = qValue(H)
    if (xvValue < 0 && lValue < 0 && hValue < 0) {
      println("EMMM")
    }
//    println(xv)
    if (xvValue > lValue && xvValue > hValue) {
      val y = k * x + b
      (xvValue, alpha.updated(p, y).updated(q, x))
    } else if (lValue > hValue) {
      val y = k * L + b
      (lValue, alpha.updated(p, y).updated(q, L))
    } else {
      val y = k * H + b
      (hValue, alpha.updated(p, y).updated(q, H))
    }
  }

  def crossProduct(x: Seq[Double], y: Seq[Double]): Double = {
    x.zip(y).map { case(l, r) => l *r }.sum
  }

  def train: Seq[Double] => Int = {
    val n = trainSet.head.features.size
    val l = trainSet.size
    var alpha = Seq.fill(l)(0D)
    var loss = 0D
    println(s"kek")
    for (iter <- 0 to 1000) {
      val p = rnd.nextInt(l - 1) + 1
      val q = rnd.nextInt(p)
      val (newLoss, newAlpha) = findMax(p, q, alpha, C)
      if (newLoss > loss) {
        alpha = newAlpha
        loss = newLoss
      } else if (newLoss < loss - 0.001) {
        println("CHE")
      }
    }
    println(loss)

    val w = (0 until n).map(i =>
      (0 until l).map(j =>
        alpha(j) * trainSet(j).answer * trainSet(j).features(i)
      ).sum
    )
    val (_, cInd) = alpha.zipWithIndex.sortBy(_._1).reverse.headOption.getOrElse(throw new IllegalStateException())
    val w0 = crossProduct(w, trainSet(cInd).features) - trainSet(cInd).answer
    testData => {
      val res: Double = (0 until l).map(i =>
        alpha(i) * trainSet(i).answer * crossProduct(testData, trainSet(i).features)
      ).sum - w0

      if (res >= 0) 1 else -1
    }
  }
}
