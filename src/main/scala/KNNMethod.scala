import Distances.Distance
import Weights.Weight

object KNNMethod {
  /**
    * @param points   object set (X) zipped with answer set (Y)
    * @param k        number of neighbours to take into account
    * @param distance metric ρ: X×X -> [0; +∞]
    * @return an algorithm a∶ X -> Y
    */
  def train(points: Seq[PointWithClass],
            k: Int,
            distance: Distance,
            weight: Weight): (Point => PointClass) =
    requestPoint => {
      val neighbours = points.sortBy(p => distance(p.point, requestPoint)).take(k) // TODO: normalize?
      val (zero, one) = neighbours.partition(_.pointClass == Zero)
      val zeroValue = zero.map(z => weight(requestPoint, z.point)).sum
      val oneValue = one.map(o => weight(requestPoint, o.point)).sum
      if (zeroValue > oneValue) Zero else One
    }
}
