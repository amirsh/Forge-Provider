package ch.epfl
package lamp
package autolifter
package library

class ForgeExtras[T](val underlying: T) extends ForgeAny {
	def unsafeImmutable: T = ???
}

trait ForgeAny

object Forge {
  def getter[T](obj: ForgeAny, field: T): T = field
  def setter[T](obj: ForgeAny, field: T, value: T): Unit = ???
}