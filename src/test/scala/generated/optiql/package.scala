package optiql.shallow

package object ops extends ch.epfl.lamp.autolifter.library.FractionalImplicits {
  def upgradeInt[R: Manifest](value: Int): R = value.asInstanceOf[R]

  def groupByHackImpl[K: Manifest, V: Manifest](self: Table[V], keySelector: V => K): Table[Tuple2[K, Table[V]]] = {
    throw new RuntimeException("groupBy not implemented")
  }

  def zeroType[T: Manifest]: T = null.asInstanceOf[T]
}