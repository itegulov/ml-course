package ru.ifmo.ctddev.ml.hw7

import java.awt.{ Color, Graphics2D }
import java.io.File
import javax.swing.JFrame

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
    val net = NeuralNetwork(trainData, Seq(784, 30, 10), sigmoid, sigmoidPrime, 10, 3.0, 2)
    val results = for (DataWithAnswer(data, answer) <- testData) yield {
      val predicted = net.predict(data).zipWithIndex.maxBy(_._1)._2
      println("GOT: " + predicted)
      println("WANTED: " + answer)
      if (predicted != answer) 0 else 1
    }
    println(results.sum.toDouble / testData.size)
//    val frame = new JFrame()
//    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
//    frame.setSize(1000, 1000)
//    frame.setVisible(true)
//
//    //Add the ubiquitous "Hello World" label.
//    val graphics = frame.getContentPane.getGraphics.asInstanceOf[Graphics2D]
//    graphics.setPaint(Color.RED)
//    graphics.fillRect(0, 0, 28 * 28, 28 * 28)
//    graphics.dispose()
//    Thread.sleep(2000)
  }
}
