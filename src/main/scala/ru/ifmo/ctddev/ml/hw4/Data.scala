package ru.ifmo.ctddev.ml.hw4

case class Data(features: Seq[Double], answer: Int) {
  require(answer == 1 || answer == -1)
}
