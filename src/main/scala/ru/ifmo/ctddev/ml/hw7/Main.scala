package ru.ifmo.ctddev.ml.hw7

object Main {
  def main(args: Array[String]): Unit = {
    val trainData = DataWithAnswer.loadFromFile(
      getClass.getResourceAsStream("/train-images-idx3-ubyte"),
      getClass.getResourceAsStream("/train-labels-idx1-ubyte"))
    val testData = DataWithAnswer.loadFromFile(
      getClass.getResourceAsStream("/t10k-images-idx3-ubyte"),
      getClass.getResourceAsStream("/t10k-labels-idx1-ubyte"))
    val sigmoid: Double => Double = x => 1D / (1D + Math.exp(-x))
    val sigmoidPrime: Double => Double = x => sigmoid(x) * (1 - sigmoid(x))
    val net = NeuralNetwork(trainData, Seq(784, 30, 10), sigmoid, sigmoidPrime, 1.0, 1)
    val results = for (DataWithAnswer(data, answer) <- testData) yield {
      val predicted = net.predict(data).zipWithIndex.maxBy(_._1)._2
      println("GOT: " + predicted)
      println("WANTED: " + answer)
      if (predicted != answer) 0 else 1
    }
    println(results.sum.toDouble / testData.size)
  }
}
