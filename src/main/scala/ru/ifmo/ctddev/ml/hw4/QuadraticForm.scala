package ru.ifmo.ctddev.ml.hw4

case class QuadraticForm(xxC: Double, xC: Double, xyC: Double, yC: Double, yyC: Double, C: Double) {
  val eps = 1e-8

  def calc(x : Double, y : Double): Double = {
    xxC * x * x + yyC * y * y + yC * y + xC * x + C + xyC * x * y
  }

  def +(other: QuadraticForm): QuadraticForm =
    QuadraticForm(xxC + other.xxC, xC + other.xC, xyC + other.xyC, yC + other.yC, yyC + other.yyC, C + other.C)

  def -(other: QuadraticForm): QuadraticForm =
    QuadraticForm(xxC - other.xxC, xC - other.xC, xyC - other.xyC, yC - other.yC, yyC - other.yyC, C - other.C)

  def *(other: QuadraticForm): QuadraticForm = {
    require(Math.abs(xxC * other.xxC) < eps)
    require(Math.abs(yyC * other.yyC) < eps)
    require(Math.abs(xxC * other.xC + other.xxC * xC) < eps)    
    require(Math.abs(yyC * other.yC + other.yyC * yC) < eps)    
    QuadraticForm(
      xxC * other.C + other.xxC * C + xC * other.xC,
      xC * other.C + other.xC * C,
      xyC * other.C + other.xyC * C + xC * other.yC + yC * other.xC,
      yC * other.C + other.yC * C,
      yyC * other.C + other.yyC * C + yC * other.yC,
      C * other.C
    )
  }

  def *(other: Double): QuadraticForm =
    QuadraticForm(xxC * other, xC * other, xyC * other, yC * other, yyC * other, C * other)

  def substituteY(k: Double, b: Double): QuadraticForm = {
    val ans = QuadraticForm(
      xxC + xyC * k + yyC * k * k,
      xC + xyC * b + yC * k + yyC * b * k * 2,
      0,
      0,
      0,
      yC * b + yyC * b * b + C
    )
    for (x <- Seq(1, 2, 3, 4)) {
      require(Math.abs(calc(x, k * x + b) - ans.calc(x, 0)) < eps)
    }
    ans
  }
}
