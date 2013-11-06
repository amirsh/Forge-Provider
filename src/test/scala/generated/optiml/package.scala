package optiml.shallow

package object ops {
  def random[T]: T = ???
  def randomGaussian: Double = ???
  implicit class IntOps(i: Int) {
    def ::(o: Int): IndexVector = ???
  }
  def max(v1: Int, v2: Int): Int = math.max(v1, v2)
  def ceil(v: Double): Int = math.ceil(v).toInt

  implicit class IndexVectorTuple2IndexVectorIndexVectorOpsCls(val self: Tuple2[IndexVector, IndexVector]) {
    def apply[T: Manifest](__arg1: (Int, Int) => T) = IndexVector.apply[T](self, __arg1)
  }

  def densematrix_raw_apply[T: Manifest](self: DenseMatrix[T], __arg0: Int): T = ???
  def densematrix_raw_update[T: Manifest](self: DenseMatrix[T], __arg0: Int, __arg1: T): Unit = ???
}