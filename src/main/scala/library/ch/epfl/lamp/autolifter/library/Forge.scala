package ch.epfl
package lamp
package autolifter
package library

class ForgeAny[T](val underlying: T) {
	def unsafeImmutable: T = ???
}