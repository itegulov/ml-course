package ru.ifmo.ctddev.ml.hw7

import java.io.{ DataInputStream, InputStream }

case class Data(grid: IndexedSeq[IndexedSeq[Double]])

case class DataWithAnswer(data: Data, answer: Int)

object DataWithAnswer {
  private def convertByte(byte: Byte): Double = {
    ((256 + byte.toInt) % 256).toDouble / 255D
  }

  def loadFromFile(imagesIs: InputStream, labelsIs: InputStream): Seq[DataWithAnswer] = {
    val imageDataIs = new DataInputStream(imagesIs)
    val labelDataIs = new DataInputStream(labelsIs)
    imageDataIs.readInt() // Magic number
    labelDataIs.readInt() // Another magic number
    val imagesCnt = imageDataIs.readInt()
    require(labelDataIs.readInt() == imagesCnt, "There should be as many images as labels")
    val rows = imageDataIs.readInt()
    val cols = imageDataIs.readInt()
    val dataMemes = for (_ <- 1 to imagesCnt) yield {
      val image = for (_ <- 1 to rows)
        yield
          for (_ <- 1 to cols)
            yield convertByte(imageDataIs.readByte())
      Data(image)
    }
    imageDataIs.close()
    val answer = for (data <- dataMemes) yield {
      DataWithAnswer(data, labelDataIs.readByte().toInt)
    }
    labelDataIs.close()
    answer
  }
}
