package ru.ifmo.ctddev.ml.hw1

import java.io.{File, FileReader}

import breeze.io.CSVReader

sealed trait PointClass

case object Zero extends PointClass
case object One extends PointClass

object PointClass {
  def fromString(string: String): PointClass = {
    if (string.toInt == 0) Zero else One
  }
}

case class Point(x: Double, y: Double) {
  def euclideanDistance(other: Point): Double =
    Math.sqrt((other.x - x) * (other.x - x) + (other.y - y) * (other.y - y))
}

case class PointWithClass(point: Point, pointClass: PointClass)

object PointWithClass {
  def parseData(csvFile: File): Seq[PointWithClass] = {
    CSVReader.read(new FileReader(csvFile)).map {
      case Seq(x, y, c) => PointWithClass(Point(x.toDouble, y.toDouble), PointClass.fromString(c))
      case _ => throw new IllegalArgumentException("CSV is malformed")
    }
  }
}
