package ch.epfl
package lamp
package autolifter
package library

object Delite {
	def single[T](block: => T): T = ???

  def composite[T](block: => T): T = ???

  def map[T1,T2,R](coll: DeliteCollection[T1], func: T1 => T2): R = ???

  def reduce[T](coll: DeliteCollection[T], zero: T, func: (T, T) => T): T = ???

  def filter[T1,T2,R](coll: DeliteCollection[T1], pred: T1 => Boolean, func: T1 => T2): R = ???

  def zip[T1,T2,T3,R](coll1: DeliteCollection[T1], coll2: DeliteCollection[T2], func: (T1, T2) => T3): R = ???

  def mapReduce[T1, T2, R](coll: DeliteCollection[T1], mapFunc: T1 => T2, zero: T2, reduceFunc: (T2, T2) => T2): R = ???

  def hashFilterReduce[T,K,V,R](coll: DeliteCollection[T], pred: T => Boolean, hash: T => K, func: T => V, zero: V, reduceFunc: (V, V) => V): R = ???

  def foreach[T](coll: DeliteCollection[T], func: T => Unit): Unit = ???
  
}

trait DeliteCollection[T] extends ForgeAny

trait ParallelCollectionBuffer[T] extends DeliteCollection[T]