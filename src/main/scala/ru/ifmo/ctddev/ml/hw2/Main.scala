package ru.ifmo.ctddev.ml.hw2

import java.awt.Color

import breeze.numerics._
import breeze.linalg._
import breeze.plot._

import scala.io.StdIn

object Main extends App {
  val initialSamples = HouseWithPrice.parseHouses(getClass.getResourceAsStream("/prices.txt"))
}
