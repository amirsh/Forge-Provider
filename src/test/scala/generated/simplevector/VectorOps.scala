package simplevector.shallow.ops

import scala.math.Ordering.Implicits._
import scala.math.Numeric.Implicits._
import ch.epfl.lamp.autolifter.library._
import ForgeArray._
import ForgeArrayBuffer._
import Numeric._

/**
 * Operations
 */

object Vector {
  def apply[T:Manifest](__arg0: Int): Vector[T] = /*allocates*/  new Vector[T](__arg0,array_empty[T](__arg0))

  protected def vector_raw_data[T:Manifest](self: Vector[T]): ForgeArray[T] = /*getter*/  self._data

  protected def vector_set_raw_data[T:Manifest](self: Vector[T],__arg1: ForgeArray[T]): Unit = /*setter*/  self._data = __arg1

  protected def vector_set_length[T:Manifest](self: Vector[T],__arg1: Int): Unit = /*setter*/  self._length = __arg1

  protected def vector_insertspace[T:Manifest](self: Vector[T],pos: Int,len: Int): Unit = Delite.single({
      vector_ensureextra(self,len)
      val data = vector_raw_data(self)
      array_copy(data,pos,data,pos+len,self.length-pos)
      vector_set_length(self,self.length+len)
    })

  protected def vector_ensureextra[T:Manifest](self: Vector[T],extra: Int): Unit = Delite.single({
      val data = vector_raw_data(self)
      if (array_length(data) - self.length < extra) {
        vector_realloc(self, self.length+extra)
      }
    })

  protected def vector_realloc[T:Manifest](self: Vector[T],minLen: Int): Unit = Delite.single({
      val data = vector_raw_data(self)
      var n = Math.max(4, array_length(data)*2).toInt
      while (n < minLen) n = n*2
      val d = array_empty[T](n)
      array_copy(data, 0, d, 0, self.length)
      vector_set_raw_data(self, d.unsafeImmutable)
    })

  protected def vector_raw_alloc[T:Manifest,R:Manifest](self: Vector[T],__arg1: Int): Vector[R] = Delite.single(Vector[R](__arg1))

  protected def vector_appendable[T:Manifest](self: Vector[T],__arg1: Int,__arg2: T): Boolean = Delite.single(true)

  protected def vector_copy[T:Manifest](self: Vector[T],__arg1: Int,__arg2: Vector[T],__arg3: Int,__arg4: Int): Unit = Delite.single({
      val src = vector_raw_data(self)
      val dest = vector_raw_data(__arg2)
      array_copy(src, __arg1, dest, __arg3, __arg4)
    })

  def foo[T:Manifest](__arg0: (Int) => T,__arg1: Int,__arg2:  => Int,__arg3: (Int,Int) => Int,__arg4: Double,__arg5: (Double) => Double): T = /*codegen*/{
      var i = 0
      val a = new Array[T](100)
      
      while (i < 100) {
        a(i) = __arg0(__arg3(__arg2,__arg2))
        a(i) = __arg0(__arg3(__arg1,__arg1))
        i += 1
      }
      println("a(5) = " + a(5))
      val z = __arg5(__arg4)
      val y = __arg2+__arg1
      println("z = " + z)
      println("y = " + y)
      a(5)
    }
}

// `extends ParallelCollectionBuffer[T]` added manually
class Vector[T:Manifest](___length: Int, ___data: ForgeArray[T]) extends ParallelCollectionBuffer[T] { self => 
  var _length = ___length
  var _data = ___data

  import Vector._
  def length(): Int = /*getter*/  self._length

  def apply(__arg0: Int): T = Delite.composite(array_apply(vector_raw_data(self), __arg0))

  def update(i: Int,e: T): Unit = Delite.composite(array_update(vector_raw_data(self), i, e))

  def slice(start: Int = 0,end: Int): Vector[T] = Delite.single({
      val out = Vector[T](end - start)
      var i = start
      while (i < end) {
        out(i-start) = self(i)
        i += 1
      }
      out
    })

  def insert(__arg0: Int,__arg1: T): Unit = Delite.single({
      vector_insertspace(self,__arg0,1)
      self(__arg0) = __arg1
    })

  def append(__arg0: Int,__arg1: T): Unit = Delite.single(self.insert(self.length, __arg1))

  def +(__arg0: Vector[T])(implicit __imp0: Numeric[T]): Vector[T] = Delite.zip[T,T,T,Vector[T]](self, __arg0, ((a,b) => a+b))

  def *(__arg0: T)(implicit __imp0: Numeric[T]): Vector[T] = Delite.map[T,T,Vector[T]](self, (e => e*__arg0))

  def sum()(implicit __imp0: Numeric[T]): T = Delite.reduce[T](self, (numeric_zero[T]), ((a,b) => a+b))

  def map[R:Manifest](__arg0: (T) => R): Vector[R] = Delite.map[T,R,Vector[R]](self, (e => __arg0(e)))

  def reduce(__arg0: (T,T) => T)(implicit __imp0: Numeric[T]): T = Delite.reduce[T](self, (numeric_zero[T]), ((a,b) => __arg0(a,b)))

  def filter(__arg0: (T) => Boolean): Vector[T] = Delite.filter[T,T,Vector[T]](self, (e => __arg0(e)), (e => e))

  def mapreduce(__arg0: (T) => T,__arg1: (T,T) => T)(implicit __imp0: Numeric[T]): T = Delite.mapReduce[T,T,T](self, (e => __arg0(e)), (numeric_zero[T]), ((a,b) => __arg1(a,b)))

  def hashreduce[K:Manifest,V:Manifest](__arg0: (T) => K,__arg1: (T) => V,__arg2: (V,V) => V)(implicit __imp0: Numeric[V]): Vector[V] = Delite.hashFilterReduce[T,K,V,Vector[V]](self, (e => true), (e => __arg0(e)), (e => __arg1(e)), (numeric_zero[V]), ((a,b) => __arg2(a,b)))

  def pprint(): Unit = Delite.foreach[T](self, (a => println(a)))

}

