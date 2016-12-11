package ru.ifmo.ctddev.ml.hw7

import java.awt._
import java.awt.event.{ActionEvent, ActionListener, MouseEvent, MouseMotionAdapter}
import java.awt.image.BufferedImage
import javax.swing.{JButton, JFrame, JPanel, WindowConstants}

import scala.collection.mutable.ArrayBuffer

class Test(trainData: Seq[DataWithAnswer]) extends JPanel {
  private val mousePositions = ArrayBuffer.empty[Point]
  private val recognizeButton = new JButton("Recognize")
  private val clearButton = new JButton("Clear")
  private val sigmoid: Double => Double = x => 1D / (1D + Math.exp(-x))
  private val sigmoidPrime: Double => Double = x => sigmoid(x) * (1 - sigmoid(x))
  private val bufferedImage = new BufferedImage(28 * 28, 28 * 28, BufferedImage.TYPE_BYTE_GRAY)
  private val bufferedImageGraphics = bufferedImage.createGraphics()
  private val smallBufferedImage = new BufferedImage(28 * 10, 28 * 10, BufferedImage.TYPE_BYTE_GRAY)
  private val smallBufferedImageGraphics = smallBufferedImage.createGraphics()
  bufferedImageGraphics.setColor(Color.WHITE)
  bufferedImageGraphics.fillRect(0, 0, 28 * 28, 28 * 28)
  bufferedImageGraphics.setColor(Color.BLACK)
  bufferedImageGraphics.drawRect(0, 0, 28 * 28, 28 * 28)
  val net = NeuralNetwork(trainData, Seq(784, 30, 10), sigmoid, sigmoidPrime, 10, 3.0, 3)

  setLayout(new BorderLayout())
  private val flowPanel = new JPanel(new FlowLayout())
  add(flowPanel, BorderLayout.PAGE_END)
  recognizeButton.setPreferredSize(new Dimension(100, 60))
  flowPanel.add(recognizeButton, BorderLayout.WEST)
  clearButton.setPreferredSize(new Dimension(100, 60))
  flowPanel.add(clearButton, BorderLayout.EAST)

  addMouseMotionListener(new MouseMotionAdapter {
    override def mouseDragged(e: MouseEvent): Unit = {
      if (e.getX < 28 * 28 && e.getY < 28 * 28) {
        println("GOTCHA")
        mousePositions += e.getPoint
        bufferedImageGraphics.setColor(Color.BLACK)
        bufferedImageGraphics.fillOval(e.getX, e.getY, 50, 50)
        repaint()
      }
    }
  })

  recognizeButton.addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      val grid: Array[Array[Double]] = Array.fill(28, 28)(0)
      val h: Int = bufferedImage.getHeight
      val w: Int = bufferedImage.getWidth
      val cost: Double = 1 / ((h.toDouble / 28D) * (w.toDouble / 28D))
      var sumi = 0
      var sumj = 0
      var cnt = 0
      for {
        i <- 0 until h
        j <- 0 until w
      } {
        if (bufferedImage.getRGB(i, j) == Color.BLACK.getRGB) {
//          grid(i * 28 / h)(j * 28 / w) += cost
          sumi += i
          sumj += j
          cnt += 1
        }
      }
      val ci: Int = sumi / cnt
      val cj: Int = sumj / cnt
      val divi: Int = Math.max(h - ci, ci)
      val divj: Int = Math.max(w - cj, cj)

      for {
        i <- 0 until h
        j <- 0 until w
      } {
        if (bufferedImage.getRGB(i, j) == Color.BLACK.getRGB) {
          grid(14 + (i - ci) * 14/ divi)(14 + (j - cj) * 14/ divj) += cost
        }
      }

      val data = Data(grid.map(_.toIndexedSeq).toIndexedSeq)
      smallBufferedImageGraphics.setColor(Color.WHITE)
      smallBufferedImageGraphics.fillRect(0, 0, 28 * 10, 28 * 10)
      for {
        i <- 0 until 28
        j <- 0 until 28
      } {
        val rgb = 1F - grid(i)(j).toFloat
        smallBufferedImageGraphics.setColor(new Color(rgb, rgb, rgb))
        smallBufferedImageGraphics.fillRect(i * 10, j * 10, 10, 10)
      }
      val predicted = net.predict(data)
      println("Predicted " + predicted)
      println("It is " + predicted.zipWithIndex.maxBy(_._1)._2)
      repaint()
    }
  })

  clearButton.addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      println("Clearing")
      bufferedImageGraphics.setColor(Color.WHITE)
      bufferedImageGraphics.fillRect(0, 0, 28 * 28, 28 * 28)
      repaint()
    }
  })

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    g.drawImage(bufferedImage, 0, 0, null)
    g.drawImage(smallBufferedImage, 0, 28 * 28 + 1, null)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val trainData = DataWithAnswer.loadFromFile(
      getClass.getResourceAsStream("/train-images-idx3-ubyte"),
      getClass.getResourceAsStream("/train-labels-idx1-ubyte"))
    val testData = DataWithAnswer.loadFromFile(
      getClass.getResourceAsStream("/t10k-images-idx3-ubyte"),
      getClass.getResourceAsStream("/t10k-labels-idx1-ubyte"))
    val frame = new JFrame()
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    val panel = new Test(trainData)
    frame.setContentPane(panel)
    frame.setSize(28 * 28, 28 * 28)
    frame.setVisible(true)
    val results = for (DataWithAnswer(data, answer) <- testData) yield {
      val predicted = panel.net.predict(data).zipWithIndex.maxBy(_._1)._2
      if (predicted != answer) 0 else 1
    }
    println(results.sum.toDouble / testData.size)
  }
}