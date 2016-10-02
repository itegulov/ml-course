package ru.ifmo.ctddev.ml.hw2

import java.io.{File, FileReader, InputStream, InputStreamReader}

import breeze.io.CSVReader

case class House(area: Long, rooms: Long)

case class HouseWithPrice(house: House, price: Long) {
  def toData: Data = Data(Seq(house.area, house.rooms), price)
}

object HouseWithPrice {
  def parseHouses(csvFile: InputStream): Seq[HouseWithPrice] = {
    CSVReader.read(new InputStreamReader(csvFile)).map {
      case Seq(area, rooms, price) => HouseWithPrice(House(area.toInt, rooms.toInt), price.toInt)
      case _ => throw new IllegalArgumentException("CSV is malformed")
    }
  }
}