package ru.ifmo.ctddev.ml.hw2

import java.io.{File, FileReader}

import breeze.io.CSVReader

case class House(area: Double, rooms: Int)

case class HouseWithPrice(house: House, price: Double)

object HouseWithPrice {
  def parseData(csvFile: File): Seq[HouseWithPrice] = {
    CSVReader.read(new FileReader(csvFile)).map {
      case Seq(area, rooms, price) => HouseWithPrice(House(area.toDouble, rooms.toInt), price.toDouble)
      case _ => throw new IllegalArgumentException("CSV is malformed")
    }
  }
}