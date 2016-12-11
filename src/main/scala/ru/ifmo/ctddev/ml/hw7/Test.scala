package ru.ifmo.ctddev.ml.hw7

import java.awt.event.{ ActionListener, MouseMotionAdapter, MouseEvent, ActionEvent }
import java.awt._
import java.awt.image.BufferedImage
import javax.swing.{ JPanel, JFrame, JButton }

import scala.collection.mutable.ArrayBuffer

class Test(trainData: Seq[DataWithAnswer]) extends JPanel {
  val mousePositions = ArrayBuffer.empty[Point]
  val button = new JButton("Recognize")
  val sigmoid: Double => Double = x => 1D / (1D + Math.exp(-x))
  val sigmoidPrime: Double => Double = x => sigmoid(x) * (1 - sigmoid(x))
  val bufferedImage = new BufferedImage(28 * 28, 28 * 28, BufferedImage.TYPE_BYTE_GRAY)
  val bufferedImageGraphics = bufferedImage.createGraphics()
  bufferedImageGraphics.setColor(Color.WHITE)
  bufferedImageGraphics.fillRect(0, 0, 28 * 28, 28 * 28)
  bufferedImageGraphics.setColor(Color.BLACK)
  bufferedImageGraphics.drawRect(0, 0, 28 * 28, 28 * 28)
  val net = NeuralNetwork(trainData, Seq(784, 30, 10), sigmoid, sigmoidPrime, 1, 3.0, 2)

  setLayout(new BorderLayout())
  add(button, BorderLayout.PAGE_END)

  button.addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      val grid : Array[Array[Double]] = Array.fill(28, 28)(0)
      val h: Int = bufferedImage.getHeight
      val w: Int = bufferedImage.getWidth
      val cost: Double = 1 / ((h.toDouble / 28D) * (w.toDouble / 28D))
      for {
        i <- 0 until h
        j <- 0 until w
      } {
        if (bufferedImage.getRGB(i, j) == Color.BLACK.getRGB) {
          grid(i * 28 / h)(j * 28 / w) += cost
        }
      }
      val data = Data(grid.map(_.toIndexedSeq).toIndexedSeq)
    }
  })

  class MyMouseMotionAdapter extends MouseMotionAdapter {
    override def mouseDragged(e: MouseEvent): Unit = {
      if (e.getX < 28 * 28 && e.getY < 28 * 28) {
        println("GOTCHA")
        mousePositions += e.getPoint
        bufferedImageGraphics.fillOval(e.getX, e.getY, 10, 10)
        repaint()
      }
    }
  }

  addMouseMotionListener(new MyMouseMotionAdapter)

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    g.drawImage(bufferedImage, 0, 0, null)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val testData = DataWithAnswer.loadFromFile(
      getClass.getResourceAsStream("/t10k-images-idx3-ubyte"),
      getClass.getResourceAsStream("/t10k-labels-idx1-ubyte"))
    val frame = new JFrame()
    val panel = new Test(testData)
    frame.setContentPane(panel)
    frame.setSize(28 * 28, 28 * 28)
    frame.setVisible(true)
  }
}