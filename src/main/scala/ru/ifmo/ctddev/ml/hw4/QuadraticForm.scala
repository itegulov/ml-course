package ru.ifmo.ctddev.ml.hw4

case class QuadraticForm(xxC: Double, xC: Double, xyC: Double, yC: Double, yyC: Double, C: Double) {

  def +(other: QuadraticForm): QuadraticForm =
    QuadraticForm(xxC + other.xxC, xC + other.xC, xyC + other.xyC, yC + other.yC, yyC + other.yyC, C + other.C)

  def -(other: QuadraticForm): QuadraticForm =
    QuadraticForm(xxC - other.xxC, xC - other.xC, xyC - other.xyC, yC - other.yC, yyC - other.yyC, C - other.C)

  def *(other: QuadraticForm): QuadraticForm =
    QuadraticForm(
      xxC * other.C + other.xxC * C + xC * other.xC,
      xC * other.C + other.xC * C,
      xyC * other.C + other.xyC * C + xC * other.yC + yC * other.xC,
      yC * other.C + other.yC * C,
      yyC * other.C + other.yyC * C + yC * other.yC,
      C * other.C
    )

  def substituteY(k: Double, b: Double): QuadraticForm =
    QuadraticForm(
      xxC + xyC * k + k * k,
      xC + xyC * b + yC * k + yyC * b * k * 2,
      0,
      0,
      0,
      yC * b + yyC * b * b + C
    )
}
