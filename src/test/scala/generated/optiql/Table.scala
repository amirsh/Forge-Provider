package optiql.shallow.ops

import scala.math.Ordering.Implicits._
import scala.math.Numeric.Implicits._
import ch.epfl.lamp.autolifter.library._
import ch.epfl.lamp.autolifter.annotations._
import ForgeArray._
import ForgeArrayBuffer._
import Numeric._

/**
 * Operations
 */

object Table {
  @mutable def apply[A: Manifest](__arg0: Int): Table[A] = /*allocates*/ new Table[A](__arg0, array_empty[A](__arg0))

  def apply[A: Manifest](__arg0: ForgeArray[A]): Table[A] = /*allocates*/ new Table[A](array_length(__arg0), __arg0)

  def apply[A: Manifest](__arg0: Seq[A]): Table[A] = /*allocates*/ new Table[A](array_length(array_fromseq(__arg0)), array_fromseq(__arg0))

  def fromFile[A: Manifest](__arg0: String, __arg1: (String) => A): Table[A] = Delite.single({
    val a = ForgeFileReader.readLines(__arg0)(__arg1)
    Table[A](a)
  })

  protected def groupByReduce[A: Manifest, K: Manifest, V: Manifest](self: Table[A], keySelector: (A) => K, valueSelector: (A) => V, reducer: (V, V) => V, condition: (A) => Boolean): Table[V] = Delite.hashFilterReduce[A, K, V, Table[V]](self, (condition), (keySelector), (valueSelector), (zeroType[V]), (reducer))

  protected def bulkDivide[A: Manifest](self: Table[A], counts: Table[Int], avgFunc: (A, Int) => A): Table[A] = Delite.zip[A, Int, A, Table[A]](self, counts, (avgFunc))

  protected def table_raw_data[A: Manifest](self: Table[A]): ForgeArray[A] = Forge.getter(self, self.data)

  protected def table_size[A: Manifest](self: Table[A]): Int = Forge.getter(self, self.size)

  protected def table_apply[A: Manifest](self: Table[A], __arg1: Int): A = Delite.composite(array_apply(table_raw_data(self), __arg1))

  protected def table_set_raw_data[A: Manifest](@write self: Table[A], __arg1: ForgeArray[A]): Unit = Forge.setter(self, self.data, __arg1)

  protected def table_set_size[A: Manifest](@write self: Table[A], __arg1: Int): Unit = Forge.setter(self, self.size, __arg1)

  protected def table_update[A: Manifest](@write self: Table[A], i: Int, e: A): Unit = Delite.composite(array_update(table_raw_data(self), i, e))

  @mutable protected def table_alloc[A: Manifest, R: Manifest](self: Table[A], __arg1: Int): Table[R] = Delite.composite(Table[R](__arg1))

  protected def table_insertspace[A: Manifest](@write self: Table[A], pos: Int, len: Int): Unit = Delite.single({
    table_ensureextra(self, len)
    val data = table_raw_data(self)
    array_copy(data, pos, data, pos + len, table_size(self) - pos)
    table_set_size(self, table_size(self) + len)
  })

  protected def table_ensureextra[A: Manifest](@write self: Table[A], extra: Int): Unit = Delite.single({
    val data = table_raw_data(self)
    if (array_length(data) - table_size(self) < extra) {
      table_realloc(self, table_size(self) + extra)
    }
  })

  protected def table_realloc[A: Manifest](@write self: Table[A], minLen: Int): Unit = Delite.single({
    val data = table_raw_data(self)
    var n = Math.max(4, array_length(data) * 2).toInt
    while (n < minLen) n = n * 2
    val d = array_empty[A](n)
    array_copy(data, 0, d, 0, table_size(self))
    table_set_raw_data(self, d.unsafeImmutable)
  })

  protected def table_appendable[A: Manifest](self: Table[A], __arg1: Int, __arg2: A): Boolean = Delite.single(true)

  protected def table_dc_append[A: Manifest](@write self: Table[A], __arg1: Int, __arg2: A): Unit = Delite.single(self.append(__arg2))

  protected def table_copy[A: Manifest](self: Table[A], __arg1: Int, @write __arg2: Table[A], __arg3: Int, __arg4: Int): Unit = Delite.single({
    val src = table_raw_data(self)
    val dest = table_raw_data(__arg2)
    array_copy(src, __arg1, dest, __arg3, __arg4)
  })

}

class Table[A: Manifest](__size: Int, __data: ForgeArray[A]) extends ParallelCollectionBuffer[A] { self =>
  var size = __size
  var data = __data

  import Table._
  def Select[R: Manifest](selector: (A) => R): Table[R] = Delite.map[A, R, Table[R]](self, (selector))

  def Where(predicate: (A) => Boolean): Table[A] = Delite.filter[A, A, Table[A]](self, (predicate), (e => e))

  def GroupBy[K: Manifest](keySelector: (A) => K): Table[Tuple2[K, Table[A]]] = Delite.single(groupByHackImpl(self, keySelector))

  def Sum[R: Numeric: Manifest](selector: (A) => R): R = Delite.mapReduce[A, R, R](self, (selector), (zeroType[R]), ((a, b) => a + b))

  def Average[R: Fractional: Numeric: Manifest](selector: (A) => R): R = Delite.single(self.Sum(selector) / upgradeInt[R](self.Count()))

  def Count(): Int = Delite.single(table_size(self))

  def First(): A = Delite.single(table_apply(self, 0))

  def Last(): A = Delite.single(table_apply(self, table_size(self) - 1))

  def OrderBy[R: Manifest](selector: (A) => R): Table[A] = Delite.single(self)

  def ThenBy[R: Manifest](selector: (A) => R): Table[A] = Delite.single(self)

  @write def insert(__arg0: Int, __arg1: A): Unit = Delite.single({
    table_insertspace(self, __arg0, 1)
    table_update(self, __arg0, __arg1)
  })

  @write def append(__arg0: A): Unit = Delite.single(self.insert(table_size(self), __arg0))

}

