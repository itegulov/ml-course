package ru.ifmo.ctddev.ml.hw2

object LinearRegression {
  def apply(areaCoefficient: Double, roomsCoefficient: Double): House => Int =
    house => Math.round(areaCoefficient * house.area + roomsCoefficient * house.rooms).toInt
}
