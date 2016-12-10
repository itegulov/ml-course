package ru.ifmo.ctddev.ml.hw7

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class NeuralNetwork(trainSet: Seq[DataWithAnswer],
                         sizes: Seq[Int],
                         sigma: Double => Double,
                         sigmaPrime: Double => Double,
                         eta: Double,
                         iterations: Int) {

  val w: Array[Array[Array[Double]]] = Array.ofDim(sizes.size - 1)
  val b: Array[Array[Double]]        = Array.ofDim(sizes.size - 1)
  init()
  for (i <- 1 to iterations) {
    trainIteration()
    println(s"ITERATION $i")
  }

  def mul(matrix: Array[Array[Double]], vector: Seq[Double]): IndexedSeq[Double] = {
    for (m <- matrix) yield {
      m.zip(vector).map { case (x, y) => x * y }.sum
    }
  }

  def init(): Unit = {
    w.indices.zip(sizes.zip(sizes.tail)).foreach { case (i, (x, y)) => w(i) = Array.ofDim(x, y) }
    for {
      l <- w.indices
      j <- w(l).indices
      k <- w(l)(j).indices
    } {
      w(l)(j)(k) = Random.nextGaussian()
    }
    b.indices.zip(sizes.tail).foreach { case (i, x) => b(i) = Array.ofDim(x) }
    for {
      l <- b.indices
      j <- b(l).indices
    } {
      b(l)(j) = Random.nextGaussian()
    }
  }

  def trainStep(d: DataWithAnswer): Unit = {
    val answer: Seq[Double] = Seq.fill[Double](sizes.last)(0).updated(d.answer, 1D)
    val (zs, as)            = calcZA(d.data)
    val deltas              = calcDeltas(d.data, as, zs, answer)
    for {
      l <- w.indices
      j <- w(l).indices
      k <- w(l)(j).indices
    } {
      w(l)(j)(k) -= eta * as(l)(j) * deltas(l)(k)
    }
    for {
      l <- b.indices
      j <- b(l).indices
    } {
      b(l)(j) -= eta * deltas(l)(j)
    }
  }

  def trainIteration(): Unit = {
    trainSet.zipWithIndex.foreach {
      case (x, i) =>
        trainStep(x)
        if (i % 1000 == 0) {
          println(s"SAMPLE $i")
        }
    }
  }

  def calcZA(d: Data): (Seq[Seq[Double]], Seq[Seq[Double]]) = {
    val ansZ = ArrayBuffer[Seq[Double]]()
    val ansA = ArrayBuffer[Seq[Double]]()
    var x    = d.grid.reduce(_ ++ _)
    ansA += x
    w.zip(b).foreach {
      case (weights, bl) =>
        val z = mul(weights.transpose, x).zip(bl).map { case (x, y) => x + y }
        ansZ += z
        x = z.map(sigma)
        ansA += x
    }
    (ansZ, ansA)
  }

  def calcDeltas(d: Data,
                 as: Seq[Seq[Double]],
                 zs: Seq[Seq[Double]],
                 answer: Seq[Double]): Seq[Seq[Double]] = {
    val ans = ArrayBuffer[Seq[Double]]()
    var delta = as.last.zip(answer).map { case (x, y) => x - y }.zip(zs.last.map(sigmaPrime)).map {
      case (x, y) => x * y
    }
    ans += delta
    sizes.indices.tail.init.reverse.foreach(l => {
      val oldDelta = delta
      delta = mul(w(l), delta).zip(zs(l - 1).map(sigmaPrime)).map { case (x, y) => x * y }
      ans += delta
    })
    ans.reverse
  }

  def predict(data: Data): Seq[Double] = {
    calcZA(data)._2.last
  }
}
