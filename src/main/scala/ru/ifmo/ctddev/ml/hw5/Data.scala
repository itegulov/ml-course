package ru.ifmo.ctddev.ml.hw5

import java.io.{File, FileReader}

import breeze.io.CSVReader


/**
  * Created by Aleksei Latyshev on 11.11.2016.
  */
case class Data(index : Int, x : Double, y : Double)

object Data {
  def parseData(csvFile : File) : Seq[Data] = {
    CSVReader.read(new FileReader(csvFile), separator = ';', skipLines = 1).map {
      case Seq(index, x, y) => Data(index.toInt, x.toDouble, y.toDouble)
      case _ => throw new IllegalArgumentException("CSV is malformed")
    }
  }
}
