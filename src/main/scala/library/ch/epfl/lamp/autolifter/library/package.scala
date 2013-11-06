package ch.epfl
package lamp
package autolifter

package object library /* extends FractionalImplicits */ {
  type ForgeArray[T] = scala.Array[T]
  type ForgeArrayBuffer[T] = scala.collection.mutable.ArrayBuffer[T]

  implicit def toForgeExtras[T](obj: T): ForgeExtras[T] = new ForgeExtras(obj)

  def fatal(msg: String) = sys.error(msg)
}
