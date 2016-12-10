package ru.ifmo.ctddev.ml.hw7

object Main {
  def main(args: Array[String]): Unit = {
    val trainData = DataWithAnswer.loadFromFile(
      getClass.getResourceAsStream("/train-images-idx3-ubyte"),
      getClass.getResourceAsStream("/train-labels-idx1-ubyte"))
    val testData = DataWithAnswer.loadFromFile(
      getClass.getResourceAsStream("/t10k-images-idx3-ubyte"),
      getClass.getResourceAsStream("/t10k-labels-idx1-ubyte"))
    val net = NeuralNetwork(trainData, Seq(784, 30, 10), x => 1D / (1D + Math.exp(x)), x => -Math.exp(x) / (Math.exp(x) + 1) * (Math.exp(x) + 1))
    for (DataWithAnswer(data, answer) <- testData) {
      println(net.predict(data))
      println("WANTED: " + answer)
    }
  }
}
