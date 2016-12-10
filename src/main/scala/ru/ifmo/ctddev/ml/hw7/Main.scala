package ru.ifmo.ctddev.ml.hw7

object Main {
  def main(args: Array[String]): Unit = {
    val trainData = DataWithAnswer.loadFromFile(
      getClass.getResourceAsStream("/train-images-idx3-ubyte"),
      getClass.getResourceAsStream("/train-labels-idx1-ubyte"))
    val testData = DataWithAnswer.loadFromFile(
      getClass.getResourceAsStream("/t10k-images-idx3-ubyte"),
      getClass.getResourceAsStream("/t10k-labels-idx1-ubyte"))
    println(trainData.size)
    println(testData.size)
  }
}
