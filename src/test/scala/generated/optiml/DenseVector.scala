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

object DenseVector {
  @mutable def apply[T: Manifest](__arg0: Int, __arg1: Boolean): DenseVector[T] = /*allocates*/ new DenseVector[T](__arg0, __arg1, array_empty[T](__arg0))

  def apply[T: Manifest](__arg0: Seq[T]): DenseVector[T] = /*allocates*/ new DenseVector[T]((__arg0.length), (true), array_fromseq(__arg0))

  protected[ops] def densevector_fromarray[T: Manifest](__arg0: ForgeArray[T], __arg1: Boolean): DenseVector[T] = /*allocates*/ new DenseVector[T](array_length(__arg0), __arg1, __arg0)

  protected[ops] def densevector_fromfunc[T: Manifest](__arg0: Int, __arg1: (Int) => T): DenseVector[T] = Delite.composite((0 :: __arg0) { i => __arg1(i) })

  def zeros(__arg0: Int): DenseVector[Double] = Delite.composite(densevector_fromfunc(__arg0, i => 0.0))

  def zerosf(__arg0: Int): DenseVector[Float] = Delite.composite(densevector_fromfunc(__arg0, i => 0f))

  def ones(__arg0: Int): DenseVector[Double] = Delite.composite(densevector_fromfunc(__arg0, i => 1.0))

  def onesf(__arg0: Int): DenseVector[Float] = Delite.composite(densevector_fromfunc(__arg0, i => 1f))

  def rand(__arg0: Int): DenseVector[Double] = Delite.composite(densevector_fromfunc(__arg0, i => random[Double]))

  def randf(__arg0: Int): DenseVector[Float] = Delite.composite(densevector_fromfunc(__arg0, i => random[Float]))

  def uniform(start: Int, step_size: Double, end: Int, isRow: Boolean = true): DenseVector[Double] = Delite.composite({
    val length = ceil((end - start) / step_size)
    densevector_fromfunc(length, i => step_size * i + start)
  })

  def flatten[T: Manifest](pieces: DenseVector[DenseVector[T]]): DenseVector[T] = Delite.single({
    if (pieces.length == 0) {
      DenseVector[T](0, pieces.isRow).unsafeImmutable
    } else {
      val sizes = pieces map { e => e.length }
      // val (total,begins) = t2(densevector_precumulate[Int](sizes, 0, (_: Rep[Int]) + (_: Rep[Int])))
      val (total, begins) = densevector_precumulate[Int](sizes, 0, (_: Int) + (_: Int))
      val result = DenseVector[T](total, pieces.isRow)
      for (i <- 0 until pieces.length) {
        result.copyFrom(begins(i), pieces(i))
      }
      result.unsafeImmutable
    }
  })

  protected[ops] def densevector_precumulate[T: Manifest](v: DenseVector[T], identity: T, func: (T, T) => T): Tuple2[T, DenseVector[T]] = Delite.composite({
    if (v.length == 0) {
      (identity, DenseVector[T](0, v.isRow).unsafeImmutable)
    } else {
      val result = DenseVector[T](0, v.isRow)
      var accum = identity
      for (i <- 0 until v.length) {
        result <<= accum
        accum = func(accum, v(i))
      }
      (accum, result.unsafeImmutable)
    }
  })

  protected[ops] def densevector_raw_alloc[R: Manifest, CR: Manifest](__arg0: CR, __arg1: Int): DenseVector[R] = Delite.composite({
    val simpleName = manifest[CR].erasure.getSimpleName
    val isRow = simpleName match {
      case s if s.startsWith("IndexVector")     => __arg0.asInstanceOf[IndexVector].isRow
      case s if s.startsWith("DenseVectorView") => __arg0.asInstanceOf[DenseVectorView[Any]].isRow
      case s if s.startsWith("DenseVector")     => __arg0.asInstanceOf[DenseVector[Any]].isRow
    }
    DenseVector[R](__arg1, isRow)
  })

  protected[ops] def densevector_sortindex_helper[T: Manifest](__arg0: Int, __arg1: Int, __arg2: ForgeArray[T])(implicit __imp0: Ordering[T]): ForgeArray[Int] = /*codegen*/ (__arg0 until __arg1: scala.Range).toArray.sortWith((a, b) => __arg2(a) < __arg2(b))
  protected[ops] def densevector_groupby_helper[T: Manifest, R: Manifest](__arg0: ForgeArray[T], __arg1: (T) => R): ForgeArray[ForgeArray[T]] = /*codegen*/ __arg0.groupBy(e => __arg1(e)).values.toArray
  protected[ops] def densevector_raw_data[T: Manifest](self: DenseVector[T]): ForgeArray[T] = Forge.getter(self, self._data)
  protected[ops] def densevector_get_length[T: Manifest](self: DenseVector[T]): Int = Forge.getter(self, self._length)
  protected[ops] def densevector_is_row[T: Manifest](self: DenseVector[T]): Boolean = Forge.getter(self, self._isRow)

  protected[ops] def densevector_set_raw_data[T: Manifest](@write self: DenseVector[T], __arg1: ForgeArray[T]): Unit = Forge.setter(self, self._data, __arg1)

  protected[ops] def densevector_set_length[T: Manifest](@write self: DenseVector[T], __arg1: Int): Unit = Forge.setter(self, self._length, __arg1)

  protected[ops] def densevector_set_isrow[T: Manifest](@write self: DenseVector[T], __arg1: Boolean): Unit = Forge.setter(self, self._isRow, __arg1)

  protected[ops] def densevector_insertspace[T: Manifest](@write self: DenseVector[T], pos: Int, len: Int): Unit = Delite.single({
    densevector_ensureextra(self, len)
    val data = densevector_raw_data(self)
    array_copy(data, pos, data, pos + len, self.length - pos)
    densevector_set_length(self, self.length + len)
  })

  protected[ops] def densevector_ensureextra[T: Manifest](@write self: DenseVector[T], extra: Int): Unit = Delite.single({
    val data = densevector_raw_data(self)
    if (array_length(data) - self.length < extra) {
      densevector_realloc(self, self.length + extra)
    }
  })

  protected[ops] def densevector_realloc[T: Manifest](@write self: DenseVector[T], minLen: Int): Unit = Delite.single({
    val data = densevector_raw_data(self)
    var n = max(4, array_length(data) * 2)
    while (n < minLen) n = n * 2
    val d = array_empty[T](n)
    array_copy(data, 0, d, 0, self.length)
    densevector_set_raw_data(self, d.unsafeImmutable)
  })

  protected[ops] def densevector_appendable[T: Manifest](self: DenseVector[T], __arg1: Int, __arg2: T): Boolean = Delite.single(true)

  protected[ops] def densevector_append[T: Manifest](@write self: DenseVector[T], __arg1: Int, __arg2: T): Unit = Delite.single(self.insert(self.length, __arg2))

  protected[ops] def densevector_copy[T: Manifest](self: DenseVector[T], __arg1: Int, @write __arg2: DenseVector[T], __arg3: Int, __arg4: Int): Unit = Delite.single({
    val src = densevector_raw_data(self)
    val dest = densevector_raw_data(__arg2)
    array_copy(src, __arg1, dest, __arg3, __arg4)
  })

  protected[ops] def densevector_densevector_filter_map[T: Manifest, R: Manifest](self: DenseVector[T], __arg1: (T) => Boolean, __arg2: (T) => R): DenseVector[R] = Delite.filter[T, R, DenseVector[R]](self, (e => __arg1(e)), (e => __arg2(e)))

}

class DenseVector[T: Manifest](___length: Int, ___isRow: Boolean, ___data: ForgeArray[T]) extends ParallelCollectionBuffer[T] { self =>
  var _length = ___length
  var _isRow = ___isRow
  var _data = ___data

  import DenseVector._
  def length(): Int = Forge.getter(self, self._length)

  def isRow(): Boolean = Forge.getter(self, self._isRow)

  def apply(__arg0: Int): T = Delite.composite(array_apply(densevector_raw_data(self), __arg0))

  def apply(__arg0: IndexVector): DenseVector[T] = Delite.composite({
    val out = __arg0.map(i => self(i))
    if (self.isRow != __arg0.isRow) out.t else out
  })

  def slice(start: Int, end: Int): DenseVector[T] = Delite.single({
    val out = DenseVector[T](end - start, self.isRow)
    for (i <- start until end) {
      out(i - start) = self(i)
    }
    out.unsafeImmutable
  })

  def t(): DenseVector[T] = /*allocates*/ new DenseVector[T](densevector_get_length(self), !(densevector_is_row(self)), array_clone(densevector_raw_data(self)))

  @write def mt(): Unit = Delite.composite(densevector_set_isrow(self, !self.isRow))

  def Clone(): DenseVector[T] = Delite.map[T, T, DenseVector[T]](self, (e => e))

  @mutable def mutable(): DenseVector[T] = Delite.single({
    val out = DenseVector[T](self.length, self.isRow)
    for (i <- 0 until out.length) {
      out(i) = self(i)
    }
    out
  })

  def replicate(__arg0: Int, __arg1: Int): DenseMatrix[T] = Delite.single({
    if (self.isRow) {
      val out = DenseMatrix[T](__arg0, __arg1 * self.length)
      for (col <- 0 until __arg1 * self.length) {
        val colToJ = col % self.length
        for (rI <- 0 until __arg0) {
          out(rI, col) = self(colToJ)
        }
      }
      out.unsafeImmutable
    } else {
      val out = DenseMatrix[T](__arg0 * self.length, __arg1)
      for (row <- 0 until __arg0 * self.length) {
        val rowToI = row % self.length
        for (cI <- 0 until __arg1) {
          out(row, cI) = self(rowToI)
        }
      }
      out.unsafeImmutable
    }
  })

  @write def update(i: Int, e: T): Unit = Delite.composite(array_update(densevector_raw_data(self), i, e))

  def <<(__arg0: T): DenseVector[T] = Delite.single({
    val out = DenseVector[T](0, self.isRow)
    out <<= self
    out <<= __arg0
    out.unsafeImmutable
  })

  def <<(__arg0: DenseVector[T]): DenseVector[T] = Delite.single({
    val out = DenseVector[T](self.length + __arg0.length, self.isRow)
    for (i <- 0 until self.length) {
      out(i) = self(i)
    }
    for (i <- 0 until __arg0.length) {
      out(i + self.length) = __arg0(i)
    }
    out.unsafeImmutable
  })

  @write def <<=(__arg0: T): Unit = Delite.composite(self.insert(self.length, __arg0))

  @write def <<=(__arg0: DenseVector[T]): Unit = Delite.composite(self.insertAll(self.length, __arg0))

  @write def insert(__arg0: Int, __arg1: T): Unit = Delite.single({
    densevector_insertspace(self, __arg0, 1)
    self(__arg0) = __arg1
  })

  @write def insertAll(__arg0: Int, __arg1: DenseVector[T]): Unit = Delite.single({
    densevector_insertspace(self, __arg0, __arg1.length)
    self.copyFrom(__arg0, __arg1)
  })

  @write def remove(__arg0: Int): Unit = Delite.composite(self.removeAll(__arg0, 1))

  @write def removeAll(pos: Int, len: Int): Unit = Delite.single({
    val data = densevector_raw_data(self)
    array_copy(data, pos + len, data, pos, self.length - (pos + len))
    densevector_set_length(self, self.length - len)
  })

  @write def copyFrom(__arg0: Int, __arg1: DenseVector[T]): Unit = Delite.single({
    val d = densevector_raw_data(self)
    for (i <- 0 until __arg1.length) {
      array_update(d, __arg0 + i, __arg1(i))
    }
  })

  @write def trim(): Unit = Delite.single({
    val data = densevector_raw_data(self)
    if (self.length < array_length(data)) {
      val d = array_empty[T](self.length)
      array_copy(data, 0, d, 0, self.length)
      densevector_set_raw_data(self, d.unsafeImmutable)
    }
  })

  @write def clear(): Unit = Delite.single({
    densevector_set_length(self, 0)
    densevector_set_raw_data(self, (array_empty[T](0)).unsafeImmutable)
  })

  @write def +=(__arg0: DenseVector[T])(implicit __imp0: Arith[T]): Unit = Delite.composite(self.indices.foreach { i => self(i) = self(i) + __arg0(i) })

  @write def +=(__arg0: T)(implicit __imp0: Arith[T]): Unit = Delite.composite(self.indices.foreach { i => self(i) = self(i) + __arg0 })

  @write def +=(__arg0: DenseVectorView[T])(implicit __imp0: Arith[T]): Unit = Delite.composite(self.indices.foreach { i => self(i) = self(i) + __arg0(i) })

  @write def *=(__arg0: DenseVector[T])(implicit __imp0: Arith[T]): Unit = Delite.composite(self.indices.foreach { i => self(i) = self(i) * __arg0(i) })

  @write def *=(__arg0: T)(implicit __imp0: Arith[T]): Unit = Delite.composite(self.indices.foreach { i => self(i) = self(i) * __arg0 })

  @write def *=(__arg0: DenseVectorView[T])(implicit __imp0: Arith[T]): Unit = Delite.composite(self.indices.foreach { i => self(i) = self(i) * __arg0(i) })

  @write def -=(__arg0: DenseVector[T])(implicit __imp0: Arith[T]): Unit = Delite.composite(self.indices.foreach { i => self(i) = self(i) - __arg0(i) })

  @write def -=(__arg0: T)(implicit __imp0: Arith[T]): Unit = Delite.composite(self.indices.foreach { i => self(i) = self(i) - __arg0 })

  @write def -=(__arg0: DenseVectorView[T])(implicit __imp0: Arith[T]): Unit = Delite.composite(self.indices.foreach { i => self(i) = self(i) - __arg0(i) })

  @write def /=(__arg0: DenseVector[T])(implicit __imp0: Arith[T]): Unit = Delite.composite(self.indices.foreach { i => self(i) = self(i) / __arg0(i) })

  @write def /=(__arg0: T)(implicit __imp0: Arith[T]): Unit = Delite.composite(self.indices.foreach { i => self(i) = self(i) / __arg0 })

  @write def /=(__arg0: DenseVectorView[T])(implicit __imp0: Arith[T]): Unit = Delite.composite(self.indices.foreach { i => self(i) = self(i) / __arg0(i) })

  def sort()(implicit __imp0: Ordering[T]): DenseVector[T] = Delite.composite({
    val v2 = self.mutable
    v2.trim()
    val a = array_sort(densevector_raw_data(v2))
    densevector_fromarray(a, self.isRow)
  })

  def sortWithIndex()(implicit __imp0: Ordering[T]): Tuple2[DenseVector[T], IndexVector] = Delite.composite({
    val sortedIndicesRaw = densevector_sortindex_helper(0, self.length, densevector_raw_data(self))
    val sortedIndices = IndexVector(densevector_fromarray(sortedIndicesRaw, self.isRow))
    (self(sortedIndices), sortedIndices)
  })

  def median()(implicit __imp0: Numeric[T], __imp1: Ordering[T]): T = Delite.single({
    val x = self.sort
    val mid = x.length / 2
    if (x.length % 2 == 0) {
      ((x(mid).asInstanceOf[Double] + x(mid - 1).asInstanceOf[Double]) / 2).asInstanceOf[T]
    } else x(mid)
  })

  def :>(__arg0: DenseVector[T])(implicit __imp0: Ordering[T]): DenseVector[Boolean] = Delite.zip[T, T, Boolean, DenseVector[Boolean]](self, __arg0, ((a, b) => a > b))

  def :<(__arg0: DenseVector[T])(implicit __imp0: Ordering[T]): DenseVector[Boolean] = Delite.zip[T, T, Boolean, DenseVector[Boolean]](self, __arg0, ((a, b) => a < b))

  def groupBy[R: Manifest](__arg0: (T) => R): DenseVector[DenseVector[T]] = Delite.composite({
    val a = densevector_groupby_helper(densevector_raw_data(self), __arg0)
    (0 :: array_length(a)) { i =>
      densevector_fromarray(array_apply(a, i), self.isRow)
    }
  })

  def toBoolean()(implicit conv: (T) => Boolean): DenseVector[Boolean] = Delite.map[T, Boolean, DenseVector[Boolean]](self, (conv))

  def toDouble()(implicit conv: (T) => Double): DenseVector[Double] = Delite.map[T, Double, DenseVector[Double]](self, (conv))

  def toFloat()(implicit conv: (T) => Float): DenseVector[Float] = Delite.map[T, Float, DenseVector[Float]](self, (conv))

  def toInt()(implicit conv: (T) => Int): DenseVector[Int] = Delite.map[T, Int, DenseVector[Int]](self, (conv))

  def indices(): IndexVector = Delite.composite(IndexVector((0), self.length, self.isRow))

  def isEmpty(): Boolean = Delite.single(self.length == 0)

  def first(): T = Delite.single(self(0))

  def last(): T = Delite.single(self(self.length - 1))

  def drop(__arg0: Int): DenseVector[T] = Delite.composite(self.slice(__arg0, self.length))

  def take(__arg0: Int): DenseVector[T] = Delite.composite(self.slice(0, __arg0))

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

  def count(__arg0: (T) => Boolean): Int = Delite.composite((densevector_densevector_filter_map(self, __arg0, (e: T) => 1)).sum)

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
