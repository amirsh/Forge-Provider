package ch.epfl
package lamp
package autolifter

package object library {
  type ForgeArray[T] = scala.Array[T]
  type ForgeArrayBuffer[T] = scala.collection.mutable.ArrayBuffer[T]

  implicit def toForgeAny[T](obj: T): ForgeAny[T] = new ForgeAny(obj)
}