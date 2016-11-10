package ru.ifmo.ctddev.ml.hw4

object SVM {

  //p == y, q == x

  def getQ(i: Int, j: Int, ai : Double, aj : Double, p: Int, q: Int): QuadraticForm = {
    if (j == p || j == q) {
      getQ(j, i, aj, ai, p, q)
    } else {
      if (i != p && i != q) {
        QuadraticForm(0, 0, 0, 0, 0, ai * aj)
      } else if (i == q) {
        j match {
          case j if j == q => QuadraticForm(1, 0, 0, 0, 0, 0)
          case j if j == p => QuadraticForm(0, 0, 1, 0, 0, 0)
          case _ => QuadraticForm(0, aj, 0, 0, 0, 0)
        }
      } else {
        j match {
          case j if j == q => QuadraticForm(0, 0, 1, 0, 0, 0)
          case j if j == p => QuadraticForm(0, 0, 0, 0, 1, 0)
          case _ => QuadraticForm(0, 0, 0, ai, 0, 0)
        }
      }
    }
  }

  def getQ(i: Int, ai : Double, p: Int, q: Int): QuadraticForm = {
    i match {
      case i if i == p => QuadraticForm(0, 1, 0, 0, 0, 0)
      case i if i == q => QuadraticForm(0, 0, 0, 1, 0, 0)
      case _ => QuadraticForm(0, 0, 0, 0, 0, ai)
    }
  }

  def findMax(p: Int, q: Int, alpha: Seq[Double], trainSet: Seq[Data], C : Double): (Double, Seq[Double]) = {
    val quad = trainSet.indices.zip(alpha).map { case (i, a) => getQ(i, a, p, q)}.reduce(_ + _)
    val minus = for { i <- trainSet.indices
          j <- trainSet.indices } yield {
      getQ(i, j, alpha(i), alpha(j), p, q) *
        crossProduct(trainSet(i).features, trainSet(j).features) *
        trainSet(i).answer *
        trainSet(j).answer
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
    val value = x * x * qq.xxC + x * qq.xC + qq.C
    val y = k * x + b
    (value, alpha.updated(p, y).updated(q, x))
  }

  def crossProduct(x: Seq[Double], y: Seq[Double]): Double = {
    x.zip(y).map { case(l, r) => l *r }.sum
  }

  def train(trainSet: Seq[Data]): Seq[Double] => Int = {
    val n = trainSet.head.features.size
    val l = trainSet.size
    var alpha = Seq.fill(n)(0D)
    for (_ <- 0 to 100) {
      val results = for {
        p <- 0 until l
        q <- 0 until l
      } yield {
        if (p != q) {
          findMax(p, q, alpha, trainSet, 3 /*wow such vanga*/)
        } else {
          (-1D, Seq.empty)
        }
      }

      val (_, newAlpha) = results.sortBy(-_._1).head
      alpha = newAlpha
    }

    val w = (0 until n).map(i =>
      (0 until l).map(j =>
        alpha(j) * trainSet(j).answer * trainSet(j).features(i)
      ).sum
    )
    val w0 = crossProduct(w, trainSet.head.features) - trainSet.head.answer
    testData => {
      val res: Double = (0 to l).map(i =>
        alpha(i) * trainSet(i).answer * crossProduct(testData, trainSet(i).features)
      ).sum - w0

      if (res >= 0) 1 else -1
    }
  }

  //  private def toNormY(p : PointClass) = {
  //    if (p == Zero) -1 else 1
  //  }

  //  def train(trainSet: Seq[PointWithClass]): Point => PointClass = {
  //    def trainOneCoordinate (lambdas : Seq[Double], trainSet : Seq[PointWithClass], index : Int, C : Double) : Seq[Double] = {
  //      val x = trainSet(index).point
  //      val y = toNormY(trainSet(index).pointClass)
  //      val newLambda = -((lambdas.zip(trainSet).map{
  //        case (lambda, p) => lambda * (p.point * x) * toNormY(p.pointClass)
  //      }.sum - x * x * lambdas(index) * y - 2) / (x * x * 2))
  //      lambdas.updated(index, newLambda)
  //    }
  //    def trainProjection(lambdas : Seq[Double], ys : Seq[Double]) : Seq[Double] = {
  //      val alpha = -lambdas.zip(ys).map{ case (x: Double, y : Double) => x * y }.sum / ys.size
  //      lambdas.zip(ys).map { case (x, y) => x + alpha * y }
  //    }
  //    def trainRec(start : Seq[Double], trainSet : Seq[PointWithClass], C : Int, iter : Int) : Seq[Double] = {
  //      if (iter == 0) {
  //        start
  //      } else {
  //        trainRec(start.indices.foldRight(start)(
  //          (i, s) => trainProjection(trainOneCoordinate(s, trainSet, i, C),
  //          trainSet.map(x => toNormY(x.pointClass)).map(_.toDouble))
  //        ), trainSet, C, iter - 1)
  //      }
  //    }
  //    val lambdas: Seq[Double] = trainRec(Seq.fill(trainSet.size)(0), trainSet, 1, 10)
  //    val wx = (for ((PointWithClass(Point(tx, _), answer), lambda) <- trainSet.zip(lambdas)) yield {
  //      lambda * toNormY(answer) * tx
  //    }).sum
  //    val wy = (for ((PointWithClass(Point(_, ty), answer), lambda) <- trainSet.zip(lambdas)) yield {
  //      lambda * toNormY(answer) * ty
  //    }).sum
  //    val w = Point(wx, wy)
  //    val w0 = w * trainSet.head.point - toNormY(trainSet.head.pointClass)
  //    point => point match {
  //      case Point(x, y) =>
  //        val res = for ((PointWithClass(Point(tx, ty), answer), lambda) <- trainSet.zip(lambdas)) yield {
  //          lambda * toNormY(answer) * (x * tx + y * ty)
  //        }
  //
  //        if (res.sum - w0 >= 0) One else Zero
  //    }
  //  }
}
