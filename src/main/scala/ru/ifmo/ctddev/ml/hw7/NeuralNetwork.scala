package ru.ifmo.ctddev.ml.hw7

import java.io.{File, PrintWriter}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class NeuralNetwork(trainSet: Seq[DataWithAnswer],
                         sizes: Seq[Int],
                         sigma: Double => Double,
                         sigmaPrime: Double => Double,
                         batchSize: Int,
                         eta: Double,
                         iterations: Int) {
  def saveToFile(f : File): Unit = {
    val out = new PrintWriter(f)
    out.println(weights.length)
    for {
      l <- weights.indices
    } {
      out.println(weights(l).length)
      for {
        j <- weights(l).indices
      } {
        out.print(weights(l)(j).length)
        weights(l)(j).foreach(x => out.print(" " + x))
        out.println()
      }
      out.println()
    }
    out.println(biases.length)
    for {
      l <- biases.indices
    } {
      out.print(biases(l).length)
      biases(l).foreach(x => out.print(" " + x))
      out.println()
    }
  }

  def loadFromFile(f : File): Unit = {

  }

  var weights: Array[Array[Array[Double]]] = Array.ofDim(sizes.size - 1)
  var biases: Array[Array[Double]]        = Array.ofDim(sizes.size - 1)
  init()
  for (i <- 1 to iterations) {
    trainIteration()
    println(s"ITERATION $i")
  }

  def mul(matrix: Array[Array[Double]], vector: Seq[Double]): IndexedSeq[Double] = {
    require(matrix.forall(_.length == matrix(0).length))
    require(matrix(0).length == vector.length)
    for (m <- matrix) yield {
      m.zip(vector).map { case (x, y) => x * y }.sum
    }
  }

  def init(): Unit = {
    weights.indices.zip(sizes.zip(sizes.tail)).foreach { case (i, (x, y)) => weights(i) = Array.ofDim(x, y) }
    for {
      l <- weights.indices
      j <- weights(l).indices
      k <- weights(l)(j).indices
    } {
      weights(l)(j)(k) = Random.nextGaussian()
    }
    biases.indices.zip(sizes.tail).foreach { case (i, x) => biases(i) = Array.ofDim(x) }
    for {
      l <- biases.indices
      j <- biases(l).indices
    } {
      biases(l)(j) = Random.nextGaussian()
    }
  }

  def trainStep(ds: Seq[DataWithAnswer]): Unit = {
    val assAndDeltas = for (d <- ds) yield {
      val answer   = Seq.fill(sizes.last)(0D).updated(d.answer, 1D)
      val (zs, as) = calcZA(d.data)
      val deltas   = calcDeltas(d.data, as, zs, answer)
      (as, deltas)
    }
    for {
      l            <- weights.indices
      j            <- weights(l).indices
      k            <- weights(l)(j).indices
      (as, deltas) <- assAndDeltas
    } {
      weights(l)(j)(k) -= eta / batchSize * as(l)(j) * deltas(l)(k)
    }
    for {
      l           <- biases.indices
      j           <- biases(l).indices
      (_, deltas) <- assAndDeltas
    } {
      biases(l)(j) -= eta / batchSize * deltas(l)(j)
    }
  }

  def trainIteration(): Unit = {
    trainSet.grouped(batchSize).zipWithIndex.foreach {
      case (x, i) =>
        trainStep(x)
        if (i % 100 == 0) {
          println(s"SAMPLE ${i * batchSize}")
        }
    }
  }

  def calcZA(d: Data): (Seq[Seq[Double]], Seq[Seq[Double]]) = {
    val zs = ArrayBuffer[Seq[Double]]()
    val activations = ArrayBuffer[Seq[Double]]()
    val x    = d.grid.reduce(_ ++ _)
    activations += x
    var activation = x
    weights.zip(biases).foreach {
      case (w, b) =>
        val z = mul(w.transpose, activation).zip(b).map { case (x, y) => x + y }
        zs += z
        activation = z.map(sigma)
        activations += activation
    }
    (zs, activations)
  }

  def calcDeltas(d: Data,
                 activations: Seq[Seq[Double]],
                 zs: Seq[Seq[Double]],
                 y: Seq[Double]): Seq[Seq[Double]] = {
    val nablaW = ArrayBuffer.empty[Seq[Double]]
    val nablaB = ArrayBuffer.empty[Seq[Double]]
    val deltas = ArrayBuffer[Seq[Double]]()
    var delta = activations.last.zip(y).map { case (x, y) => x - y }.zip(zs.last.map(sigmaPrime)).map {
      case (x, y) => x * y
    }
    nablaB += delta
    deltas += delta
    sizes.indices.tail.init.reverse.foreach(l => {
      val sp = zs(l - 1).map(sigmaPrime)
      delta = mul(weights(l), delta).zip(sp).map { case (x, y) => x * y }
      deltas += delta
    })
    deltas.reverse
  }

  def predict(data: Data): Seq[Double] = {
    calcZA(data)._2.last
  }
}
