package ru.ifmo.ctddev.ml.hw2

case class Data(features: Seq[Double], answer: Double)

object Data {
  def normalize(list : Seq[Data]): Seq[Data] = {
    def rec(lis : Seq[Seq[Double]]) : Seq[Seq[Double]] = {
      if (lis.head.isEmpty) {
        Seq()
      } else {
        normalizeDoubles(lis.map(_.head)) +: rec(lis.map(_.tail))
      }
    }
    rec(list.map(_.features)).zip(normalizeDoubles(list.map(_.answer))).map{
      case (list1, list2) => Data(list1, list2)
    }
  }

  def normalizeDoubles(list : Seq[Double]) : Seq[Double] = {
    val mean = list.sum / list.size
    val sigma = list.map(x => (x - mean) * (x - mean)).sum
    list.map(x => (x - mean) / sigma)
  }
}
