package ru.ifmo.ctddev.ml.hw7

import java.awt._
import java.awt.event.{ ActionEvent, ActionListener, MouseMotionAdapter, MouseEvent }
import java.awt.image.BufferedImage
import java.io.File
import javax.swing.{ JPanel, JFrame, JButton, WindowConstants }

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

class Test(trainData: Seq[DataWithAnswer]) extends JPanel {
  private val mousePositions                 = ArrayBuffer.empty[Point]
  private val recognizeButton                = new JButton("Recognize")
  private val clearButton                    = new JButton("Clear")
  private val sigmoid: Double => Double      = x => 1D / (1D + Math.exp(-x))
  private val sigmoidPrime: Double => Double = x => sigmoid(x) * (1 - sigmoid(x))
  private val bufferedImage                  = new BufferedImage(28 * 28, 28 * 28, BufferedImage.TYPE_BYTE_GRAY)
  private val bufferedImageGraphics          = bufferedImage.createGraphics()
  private val smallBufferedImage =
    new BufferedImage(28 * 10, 28 * 10, BufferedImage.TYPE_BYTE_GRAY)
  private val smallBufferedImageGraphics = smallBufferedImage.createGraphics()
  bufferedImageGraphics.setColor(Color.WHITE)
  bufferedImageGraphics.fillRect(0, 0, 28 * 28, 28 * 28)
  bufferedImageGraphics.setColor(Color.BLACK)
  bufferedImageGraphics.drawRect(0, 0, 28 * 28, 28 * 28)
  val net = NeuralNetwork(trainData,
                          Seq(784, 30, 10),
                          sigmoid,
                          sigmoidPrime,
                          10,
                          3.0,
                          3,
                          fileOpt = Some(new File("memes_2016-12-11T12:06:12.814.txt")))

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
        mousePositions += e.getPoint
        bufferedImageGraphics.setColor(Color.BLACK)
        bufferedImageGraphics.fillOval(e.getX, e.getY, 50, 50)
        repaint()
      }
    }
  })

  recognizeButton.addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      val grid = compress(centralize)

      val data = Data(grid.map(_.toIndexedSeq).toIndexedSeq)
      smallBufferedImageGraphics.setColor(Color.WHITE)
      smallBufferedImageGraphics.fillRect(0, 0, 28 * 10, 28 * 10)
      for {
        i <- 0 until 28
        j <- 0 until 28
      } {
        val rgb = 1F - grid(i)(j).toFloat
        smallBufferedImageGraphics.setColor(new Color(rgb, rgb, rgb))
        smallBufferedImageGraphics.fillRect(j * 10, i * 10, 10, 10)
      }
      val predicted = net.predict(data)
      println("Predicted " + predicted)
      println("It is " + predicted.zipWithIndex.maxBy(_._1)._2)
      net.trainStep(Seq(DataWithAnswer(data, StdIn.readInt())))
      repaint()
    }
  })

  clearButton.addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      bufferedImageGraphics.setColor(Color.WHITE)
      bufferedImageGraphics.fillRect(0, 0, 28 * 28, 28 * 28)
      repaint()
    }
  })

  def centralize: Array[Array[Int]] = {
    val h: Int                        = bufferedImage.getHeight
    val w: Int                        = bufferedImage.getWidth
    var sumi                          = 0
    var sumj                          = 0
    var cnt                           = 0
    for {
      i <- 0 until h
      j <- 0 until w
    } {
      if (bufferedImage.getRGB(i, j) == Color.BLACK.getRGB) {
        sumi += i
        sumj += j
        cnt += 1
      }
    }
    val ci: Int   = sumi / cnt
    val cj: Int   = sumj / cnt
    val grid = Array.fill(h, w)(0)
    for {
      i <- 0 until h
      j <- 0 until w
    } {
      if (bufferedImage.getRGB(i, j) == Color.BLACK.getRGB) {
        val ni = i - ci + h / 2
        val nj = j - cj + w / 2
        if (ni >= 0 && ni < h && nj >= 0 && nj < w) {
          grid(ni)(nj) = 1
        }
      }
    }
    grid
  }

  def compress(grid: Array[Array[Int]]): Array[Array[Double]] = {
    val answer = Array.ofDim[Double](28, 28)
    val cost = 1D / (grid.length / 28 * grid(0).length / 28)
    for {
      i <- grid.indices
      j <- grid(i).indices
    } {
      answer(j * 28 / grid(0).length)(i * 28 / grid.length) += grid(i)(j) * cost
    }
    answer
  }

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    g.drawImage(bufferedImage, 0, 0, null)
    g.drawImage(smallBufferedImage, 0, 28 * 28 + 1, null)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val frame = new JFrame()
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    val panel = new Test(Seq(DataWithAnswer(Data(IndexedSeq.empty), 0), DataWithAnswer(Data(IndexedSeq.empty), 0)))
    frame.setContentPane(panel)
    frame.setSize(28 * 28, 28 * 28)
    frame.setVisible(true)
  }
}
