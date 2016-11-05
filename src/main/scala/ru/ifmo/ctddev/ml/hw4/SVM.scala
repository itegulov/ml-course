package ru.ifmo.ctddev.ml.hw4

import ru.ifmo.ctddev.ml.hw1.{Point, PointClass, PointWithClass, Zero}

object SVM {
  def train(trainSet: Seq[PointWithClass]): Point => PointClass = {
    def trainOneCoordinate (lambdas : Seq[Double], trainSet : Seq[PointWithClass], index : Int, C : Double) : Seq[Double] = {
      val x = trainSet(index).point
      def toNormY(p : PointClass) = {
        if (p == Zero) -1 else 1
      }
      val y = toNormY(trainSet(index).pointClass)
      val newLambda = -((lambdas.zip(trainSet).map{
        case (lambda, p) => lambda * (p.point * x) * toNormY(p.pointClass)
      }.sum - x * x * lambdas(index) * y - 2) / (x * x * 2))
      lambdas.updated(index, if (newLambda > C) C else if (newLambda < 0) 0 else newLambda)
    }
    def trainProjection(lambdas : Seq[Double]) : Seq[Double] = {
      ???
    }
    def trainRec(start : Seq[Double], trainSet : Seq[PointWithClass], C : Int, iter : Int) : Seq[Double] = {
      if (iter == 0) {
        start
      } else {
        trainRec(start.indices.foldRight(start)((i, s) => trainProjection(trainOneCoordinate(s, trainSet, i, C))),
          trainSet, C, iter - 1)
      }
    }
    val lambdas = trainRec(trainProjection(Seq.fill(trainSet.size){0}), trainSet, ???, 2000)
    ???
  }
}
