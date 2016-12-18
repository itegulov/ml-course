package ru.ifmo.ctddev.ml.hw7

import java.awt.{ Color, Graphics2D }
import java.io.File
import javax.swing.JFrame

import scala.collection.mutable

object Main {
  def main(args: Array[String]): Unit = {
//    val trainData = DataWithAnswer.loadFromFile(
//      getClass.getResourceAsStream("/train-images-idx3-ubyte"),
//      getClass.getResourceAsStream("/train-labels-idx1-ubyte"))
    val testData = DataWithAnswer.loadFromFile(
      getClass.getResourceAsStream("/t10k-images-idx3-ubyte"),
      getClass.getResourceAsStream("/t10k-labels-idx1-ubyte"))
    val sigmoid: Double => Double = x => 1D / (1D + Math.exp(-x))
    val sigmoidPrime: Double => Double = x => sigmoid(x) * (1 - sigmoid(x))
    val map = mutable.Map.empty[Int, (Int, Int)]
    for (i <- 0 to 9) {
      map(i) = (0, 0)
    }
    val net = NeuralNetwork(testData, Seq(784, 30, 10), sigmoid, sigmoidPrime, 10, 3.0, 15, fileOpt = Some(new File("memes_2016-12-11T12:06:12.814.txt")))
    val results = for (DataWithAnswer(data, answer) <- testData) yield {
      val predicted = net.predict(data).zipWithIndex.maxBy(_._1)._2
      if (predicted != answer) {
        val (good, bad) = map(answer)
        map(answer) = (good, bad + 1)
        0
      } else {
        val (good, bad) = map(answer)
        map(answer) = (good + 1, bad)
        1
      }
    }
    println(results.sum.toDouble / testData.size)
    println(map.mkString("\n"))
    for ((k, (good, bad)) <- map) {
      println(k + ": " + good + ", " + bad)
    }
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
