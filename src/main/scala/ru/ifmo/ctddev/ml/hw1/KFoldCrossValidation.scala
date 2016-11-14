package ru.ifmo.ctddev.ml.hw1

import scala.util.Random

object KFoldCrossValidation {
  private def cut[A](xs: Seq[A], n: Int) = {
    val (quot, rem) = (xs.size / n, xs.size % n)
    val (smaller, bigger) = xs.splitAt(xs.size - rem * (quot + 1))
    smaller.grouped(quot) ++ bigger.grouped(quot + 1)
  }

  def apply[A](samples: Seq[PointWithClass], k: Int): Seq[(Seq[PointWithClass], Seq[PointWithClass])] = {
    val parts = cut(new Random(2837423648L).shuffle(samples), k).toSeq
    for (i <- parts.indices)
      yield (parts.patch(i, Nil, 1).flatten, parts(i))
  }
}
