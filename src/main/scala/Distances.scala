import Math.pow

object Distances {
  type Distance = (Point, Point) => Double

  def minkowski(p: Int): Distance =
    (f, s) => pow(pow(f.x - s.x, p) + pow(f.y - s.y, p), 1.0D / p)

  val manhattan: Distance = minkowski(1)

  val euclidean: Distance = minkowski(2)
}
