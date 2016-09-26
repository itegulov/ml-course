package ru.ifmo.ctddev.ml.hw2

object LinearRegression {
  def apply(areaCoefficient: Double, roomsCoefficient: Double): House => Double =
    house => areaCoefficient * house.area + roomsCoefficient * house.rooms
}
