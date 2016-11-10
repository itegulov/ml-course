package ru.ifmo.ctddev.ml.hw4

import ru.ifmo.ctddev.ml.hw1._

object SVM {

  def findMax(p: Int, q: Int, alpha: Seq[Double], trainSet: Seq[Data]): (Double, Seq[Double]) = {
    ???
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
          findMax(p, q, alpha, trainSet)
        } else {
          (-1, Seq.empty)
        }
      }

      val (_, newAlpha) = results.sortBy { case ((qValue, _)) => -qValue }.head
      alpha = newAlpha
    }

    val w = for (i <- 0 to n) {
      for (j <- 0 to l) {
        ???
      }
    }
    ???
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
