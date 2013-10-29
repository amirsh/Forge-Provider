package ch.epfl
package lamp
package autolifter
package library

object Numeric {
  def numeric_zero[T](implicit num: Numeric[T]): T = num.zero
}
