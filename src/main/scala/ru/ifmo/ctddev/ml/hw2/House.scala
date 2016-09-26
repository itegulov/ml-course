package ru.ifmo.ctddev.ml.hw2

import java.io.{File, FileReader}

import breeze.io.CSVReader

case class House(area: Int, rooms: Int)

case class HouseWithPrice(house: House, price: Int)

object HouseWithPrice {
  def parseData(csvFile: File): Seq[HouseWithPrice] = {
    CSVReader.read(new FileReader(csvFile)).map {
      case Seq(area, rooms, price) => HouseWithPrice(House(area.toInt, rooms.toInt), price.toInt)
      case _ => throw new IllegalArgumentException("CSV is malformed")
    }
  }
}