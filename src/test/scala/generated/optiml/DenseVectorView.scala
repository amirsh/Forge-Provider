package optiml.shallow.ops

import scala.math.Ordering.Implicits._
import scala.math.Numeric.Implicits._
import ch.epfl.lamp.autolifter.library._
import ch.epfl.lamp.autolifter.annotations._
import ForgeArray._
import ForgeArrayBuffer._
import Numeric._
import Arith._
import Stringable._

/**
 * Operations
 */

object DenseVectorView {
  def apply[T: Manifest](__arg0: ForgeArray[T], __arg1: Int, __arg2: Int, __arg3: Int, __arg4: Boolean): DenseVectorView[T] = /*allocates*/ new DenseVectorView[T](__arg0, __arg1, __arg2, __arg3, __arg4)

  protected[ops] def densevectorview_data[T: Manifest](self: DenseVectorView[T]): ForgeArray[T] = Forge.getter(self, self._data)

  protected[ops] def densevectorview_start[T: Manifest](self: DenseVectorView[T]): Int = Forge.getter(self, self._start)

  protected[ops] def densevectorview_stride[T: Manifest](self: DenseVectorView[T]): Int = Forge.getter(self, self._stride)

  @simple protected[ops] def densevectorview_illegalalloc[T: Manifest](self: DenseVectorView[T], __arg1: Int): Nothing = Delite.composite(fatal("DenseVectorViews cannot be allocated from a parallel op"))

  @simple protected[ops] def densevectorview_illegalupdate[T: Manifest](self: DenseVectorView[T], __arg1: Int, __arg2: T): Nothing = Delite.composite(fatal("DenseVectorViews cannot be updated"))

  protected[ops] def densevectorview_densevector_filter_map[T: Manifest, R: Manifest](self: DenseVectorView[T], __arg1: (T) => Boolean, __arg2: (T) => R): DenseVector[R] = Delite.filter[T, R, DenseVector[R]](self, (e => __arg1(e)), (e => __arg2(e)))

}

class DenseVectorView[T: Manifest](___data: ForgeArray[T], ___start: Int, ___stride: Int, ___length: Int, ___isRow: Boolean) extends ParallelCollection[T] { self =>
  var _data = ___data
  var _start = ___start
  var _stride = ___stride
  var _length = ___length
  var _isRow = ___isRow

  import DenseVectorView._
  def length(): Int = Forge.getter(self, self._length)

  def isRow(): Boolean = Forge.getter(self, self._isRow)

  def apply(__arg0: Int): T = Delite.composite(array_apply(densevectorview_data(self), densevectorview_start(self) + __arg0 * densevectorview_stride(self)))

  def slice(start: Int, end: Int): DenseVectorView[T] = Delite.composite(DenseVectorView(densevectorview_data(self), densevectorview_start(self) + start * densevectorview_stride(self), densevectorview_stride(self), end - start, self.isRow))

  def toDense(): DenseVector[T] = Delite.composite(self.map(e => e))

  def toBoolean()(implicit conv: (T) => Boolean): DenseVector[Boolean] = Delite.map[T, Boolean, DenseVector[Boolean]](self, (conv))

  def toDouble()(implicit conv: (T) => Double): DenseVector[Double] = Delite.map[T, Double, DenseVector[Double]](self, (conv))

  def toFloat()(implicit conv: (T) => Float): DenseVector[Float] = Delite.map[T, Float, DenseVector[Float]](self, (conv))

  def toInt()(implicit conv: (T) => Int): DenseVector[Int] = Delite.map[T, Int, DenseVector[Int]](self, (conv))

  def indices(): IndexVector = Delite.composite(IndexVector((0), self.length, self.isRow))

  def isEmpty(): Boolean = Delite.single(self.length == 0)

  def first(): T = Delite.single(self(0))

  def last(): T = Delite.single(self(self.length - 1))

  def drop(__arg0: Int): DenseVectorView[T] = Delite.composite(self.slice(__arg0, self.length))

  def take(__arg0: Int): DenseVectorView[T] = Delite.composite(self.slice(0, __arg0))

  def contains(__arg0: T): Boolean = Delite.single({
    var found = false
    var i = 0
    while (i < self.length && !found) {
      if (self(i) == __arg0) {
        found = true
      }
      i += 1
    }
    found
  })

  def distinct(): DenseVector[T] = Delite.single({
    val out = DenseVector[T](0, self.isRow)
    for (i <- 0 until self.length) {

      if (!out.contains(self(i))) out <<= self(i)
    }
    out.unsafeImmutable
  })

  def makeString()(implicit __imp0: Stringable[T]): String = Delite.single({
    var s = ""
    if (self.length == 0) {
      "[ ]"
    } else if (self.isRow) {
      s = s + "["
      for (i <- 0 until self.length - 1) {
        s = s + self(i).makeStr + " "
      }
      s = s + self(self.length - 1).makeStr
      s = s + "]"
    } else {
      for (i <- 0 until self.length - 1) {
        s = s + "[" + self(i).makeStr + "]\n"
      }
      s = s + "[" + self(self.length - 1).makeStr + "]"
    }
    s
  })

  override def toString(): String = Delite.single({
    var s = ""
    if (self.length == 0) {
      "[ ]"
    } else if (self.isRow) {
      s = s + "["
      for (i <- 0 until self.length - 1) {
        s = s + self(i) + " "
      }
      s = s + self(self.length - 1)
      s = s + "]"
    } else {
      for (i <- 0 until self.length - 1) {
        s = s + "[" + self(i) + "]\n"
      }
      s = s + "[" + self(self.length - 1) + "]"
    }
    s
  })

  @simple def pprint()(implicit __imp0: Stringable[T]): Unit = Delite.composite(println(self.makeStr))

  def +(__arg0: DenseVector[T])(implicit __imp0: Arith[T]): DenseVector[T] = Delite.zip[T, T, T, DenseVector[T]](self, __arg0, ((a, b) => a + b))

  def -(__arg0: DenseVector[T])(implicit __imp0: Arith[T]): DenseVector[T] = Delite.zip[T, T, T, DenseVector[T]](self, __arg0, ((a, b) => a - b))

  def *(__arg0: DenseVector[T])(implicit __imp0: Arith[T]): DenseVector[T] = Delite.zip[T, T, T, DenseVector[T]](self, __arg0, ((a, b) => a * b))

  def *:*(__arg0: DenseVector[T])(implicit __imp0: Arith[T]): T = Delite.composite({
    if (self.length != __arg0.length) fatal("dimension mismatch: vector dot product")
    (self * __arg0).sum
  })

  def **(__arg0: DenseVector[T])(implicit __imp0: Arith[T]): DenseMatrix[T] = Delite.composite({
    if (self.isRow || !__arg0.isRow) fatal("dimension mismatch: vector outer product")
    val out = DenseMatrix[T](self.length, __arg0.length)
    for (i <- 0 until self.length) {
      for (j <- 0 until __arg0.length) {
        out(i, j) = self(i) * __arg0(j)
      }
    }
    out.unsafeImmutable
  })

  def /(__arg0: DenseVector[T])(implicit __imp0: Arith[T]): DenseVector[T] = Delite.zip[T, T, T, DenseVector[T]](self, __arg0, ((a, b) => a / b))

  def +(__arg0: DenseVectorView[T])(implicit __imp0: Arith[T]): DenseVector[T] = Delite.zip[T, T, T, DenseVector[T]](self, __arg0, ((a, b) => a + b))

  def -(__arg0: DenseVectorView[T])(implicit __imp0: Arith[T]): DenseVector[T] = Delite.zip[T, T, T, DenseVector[T]](self, __arg0, ((a, b) => a - b))

  def *(__arg0: DenseVectorView[T])(implicit __imp0: Arith[T]): DenseVector[T] = Delite.zip[T, T, T, DenseVector[T]](self, __arg0, ((a, b) => a * b))

  def *:*(__arg0: DenseVectorView[T])(implicit __imp0: Arith[T]): T = Delite.composite({
    if (self.length != __arg0.length) fatal("dimension mismatch: vector dot product")
    (self * __arg0).sum
  })

  def **(__arg0: DenseVectorView[T])(implicit __imp0: Arith[T]): DenseMatrix[T] = Delite.composite({
    if (self.isRow || !__arg0.isRow) fatal("dimension mismatch: vector outer product")
    val out = DenseMatrix[T](self.length, __arg0.length)
    for (i <- 0 until self.length) {
      for (j <- 0 until __arg0.length) {
        out(i, j) = self(i) * __arg0(j)
      }
    }
    out.unsafeImmutable
  })

  def /(__arg0: DenseVectorView[T])(implicit __imp0: Arith[T]): DenseVector[T] = Delite.zip[T, T, T, DenseVector[T]](self, __arg0, ((a, b) => a / b))

  def zip[B: Manifest, R: Manifest](__arg0: DenseVector[B])(__arg1: (T, B) => R): DenseVector[R] = Delite.zip[T, B, R, DenseVector[R]](self, __arg0, ((a, b) => __arg1(a, b)))

  def zip[B: Manifest, R: Manifest](__arg0: DenseVectorView[B])(__arg1: (T, B) => R): DenseVector[R] = Delite.zip[T, B, R, DenseVector[R]](self, __arg0, ((a, b) => __arg1(a, b)))

  def +(__arg0: T)(implicit __imp0: Arith[T]): DenseVector[T] = Delite.map[T, T, DenseVector[T]](self, (e => e + __arg0))

  def -(__arg0: T)(implicit __imp0: Arith[T]): DenseVector[T] = Delite.map[T, T, DenseVector[T]](self, (e => e - __arg0))

  def *(__arg0: T)(implicit __imp0: Arith[T]): DenseVector[T] = Delite.map[T, T, DenseVector[T]](self, (e => e * __arg0))

  def *(__arg0: DenseMatrix[T])(implicit __imp0: Arith[T]): DenseVector[T] = Delite.composite({
    if (!self.isRow) fatal("dimension mismatch: vector * matrix")
    __arg0.t.mapRowsToVector { row => self *:* row }
  })

  def /(__arg0: T)(implicit __imp0: Arith[T]): DenseVector[T] = Delite.map[T, T, DenseVector[T]](self, (e => e / __arg0))

  def abs()(implicit __imp0: Arith[T]): DenseVector[T] = Delite.map[T, T, DenseVector[T]](self, (e => e.abs))

  def exp()(implicit __imp0: Arith[T]): DenseVector[T] = Delite.map[T, T, DenseVector[T]](self, (e => e.exp))

  def log()(implicit __imp0: Arith[T]): DenseVector[T] = Delite.map[T, T, DenseVector[T]](self, (e => e.log))

  def sum()(implicit __imp0: Arith[T]): T = Delite.reduce[T](self, (implicitly[Arith[T]].zero(self((0)))), ((a, b) => a + b))

  def mean()(implicit conv: (T) => Double): Double = Delite.composite(self.map(conv).sum / self.length)

  def min()(implicit __imp0: Ordering[T]): T = Delite.reduce[T](self, (self(0)), ((a, b) => if (a < b) a else b))

  def max()(implicit __imp0: Ordering[T]): T = Delite.reduce[T](self, (self(0)), ((a, b) => if (a > b) a else b))

  def minIndex()(implicit __imp0: Ordering[T]): Int = Delite.single({
    var min = self(0)
    var minIndex = 0
    for (i <- 0 until self.length) {
      if (self(i) < min) {
        min = self(i)
        minIndex = i
      }
    }
    minIndex
  })

  def maxIndex()(implicit __imp0: Ordering[T]): Int = Delite.single({
    var max = self(0)
    var maxIndex = 0
    for (i <- 0 until self.length) {
      if (self(i) > max) {
        max = self(i)
        maxIndex = i
      }
    }
    maxIndex
  })

  def map[R: Manifest](__arg0: (T) => R): DenseVector[R] = Delite.map[T, R, DenseVector[R]](self, (e => __arg0(e)))

  def reduce(__arg0: (T, T) => T)(implicit __imp0: Arith[T]): T = Delite.reduce[T](self, (implicitly[Arith[T]].zero(self((0)))), ((a, b) => __arg0(a, b)))

  def filter(__arg0: (T) => Boolean): DenseVector[T] = Delite.filter[T, T, DenseVector[T]](self, (e => __arg0(e)), (e => e))

  def foreach(__arg0: (T) => Unit): Unit = Delite.foreach[T](self, (e => __arg0(e)))

  def find(__arg0: (T) => Boolean): IndexVector = Delite.composite(IndexVector(self.indices.filter(i => __arg0(self(i)))))

  def count(__arg0: (T) => Boolean): Int = Delite.composite((densevectorview_densevector_filter_map(self, __arg0, (e: T) => 1)).sum)

  def partition(pred: (T) => Boolean): Tuple2[DenseVector[T], DenseVector[T]] = Delite.single({
    val outT = DenseVector[T](0, self.isRow)
    val outF = DenseVector[T](0, self.isRow)
    for (i <- 0 until self.length) {
      val x = self(i)
      if (pred(x)) outT <<= x
      else outF <<= x
    }
    (outT.unsafeImmutable, outF.unsafeImmutable)
  })

  def flatMap[R: Manifest](__arg0: (T) => DenseVector[R]): DenseVector[R] = Delite.composite(DenseVector.flatten(self.map(__arg0)))

  def scan[R: Manifest](zero: R)(__arg1: (R, T) => R): DenseVector[R] = Delite.single({
    val out = DenseVector[R](self.length, self.isRow)
    out(0) = __arg1(zero, self(0))
    var i = 1
    while (i < self.length) {
      out(i) = __arg1(out(i - 1), self(i))
      i += 1
    }
    out.unsafeImmutable
  })

  def prefixSum()(implicit __imp0: Arith[T]): DenseVector[T] = Delite.composite(self.scan(implicitly[Arith[T]].zero(self((0))))((a, b) => a + b))

}
