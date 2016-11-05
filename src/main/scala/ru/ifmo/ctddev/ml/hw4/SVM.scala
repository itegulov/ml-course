package ru.ifmo.ctddev.ml.hw4

import ru.ifmo.ctddev.ml.hw1.{Point, PointClass, PointWithClass, Zero}

object SVM {
  def train(trainSet: Seq[PointWithClass]): Point => PointClass = {
    def trainOneCoordinate (lambdas : Seq[Double], trainSet : Seq[PointWithClass], index : Int) : Seq[Double] = {
      val x = trainSet(index).point
      def toNormY(p : PointClass) = {
        if (p == Zero) -1 else 1
      }
      val y = toNormY(trainSet(index).pointClass)
      val newLambda = -((lambdas.zip(trainSet).map{
        case (lambda, p) => lambda * (p.point * x) * toNormY(p.pointClass)
      }.sum - x * x * lambdas(index) * y - 2) / (x * x * 2))
      lambdas.updated(index, newLambda)
    }
    def trainProjection(lambdas : Seq[Double], trainSet : Seq[PointWithClass]) : Seq[Double] = {
      ???
    }
    def trainRec(start : Seq[Double], trainSet : Seq[PointWithClass], iter : Int) : Seq[Double] = {
      ???
    }
    val lambdas = trainRec(Seq.fill(trainSet.size){0}, trainSet, 2000)
    ???
  }
}
