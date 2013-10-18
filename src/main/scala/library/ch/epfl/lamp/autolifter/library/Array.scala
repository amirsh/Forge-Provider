package ch.epfl
package lamp
package autolifter
package library

object ForgeArray {
  def array_empty[T:Manifest](__arg0: Int): ForgeArray[T]
    = new ForgeArray[T](__arg0)
  def array_empty_imm[T:Manifest](__arg0: Int): ForgeArray[T]
    = array_empty[T](__arg0)
  def array_copy[T:Manifest](__arg0: ForgeArray[T],__arg1: Int,__arg2: ForgeArray[T],__arg3: Int,__arg4: Int): Unit
    = System.arraycopy(__arg0,__arg1,__arg2,__arg3,__arg4)
  def array_update[T:Manifest](__arg0: ForgeArray[T],__arg1: Int,__arg2: T): Unit
    = __arg0(__arg1) = __arg2
  def array_apply[T:Manifest](__arg0: ForgeArray[T],__arg1: Int): T
    = __arg0(__arg1)
  def array_length[T:Manifest](__arg0: ForgeArray[T]): Int
    = __arg0.length
  def array_clone[T:Manifest](__arg0: ForgeArray[T]): ForgeArray[T]
    = __arg0.clone
  def array_sort[T:Manifest:Ordering](__arg0: ForgeArray[T]): ForgeArray[T] = {
    val d = array_empty[T](__arg0.length)
    array_copy(__arg0,0,d,0,__arg0.length)
    scala.util.Sorting.quickSort(d)
    d
  }
  def array_fromseq[T:Manifest](__arg0: Seq[T]): ForgeArray[T]
    = __arg0.toArray
  def array_string_split(__arg0: String, __arg1: String): ForgeArray[String]
    = __arg0.split(__arg1)

  def scala_array_apply[T:Manifest](__arg0: Array[T],__arg1: Int): T
    = array_apply(__arg0,__arg1)
  def scala_array_length[T:Manifest](__arg0: Array[T]): Int
    = array_length(__arg0)
}

object ForgeArrayBuffer {
  def array_buffer_empty[T:Manifest](__arg0: Int): ForgeArrayBuffer[T]
    = (new scala.collection.mutable.ArrayBuffer[T]()) ++ (new Array[T](__arg0))
  def array_buffer_copy[T](src: ForgeArrayBuffer[T], srcPos: Int, dest: ForgeArrayBuffer[T], destPos: Int, length: Int): Unit = {
    for (i <- 0 until length) {
      dest(destPos+i) = src(srcPos+i)
    }
  }
  def array_buffer_update[T:Manifest](__arg0: ForgeArrayBuffer[T],__arg1: Int,__arg2: T): Unit
    = __arg0(__arg1) = __arg2
  def array_buffer_apply[T:Manifest](__arg0: ForgeArrayBuffer[T],__arg1: Int): T
    = __arg0(__arg1)
  def array_buffer_length[T:Manifest](__arg0: ForgeArrayBuffer[T]): Int
    = __arg0.length
  def array_buffer_set_length[T:Manifest](__arg0: ForgeArrayBuffer[T],__arg1: Int): Unit
    = __arg0.slice(0,__arg1)
  def array_buffer_append[T:Manifest](__arg0: ForgeArrayBuffer[T],__arg1: T): Unit
    = { __arg0 += __arg1 }
  def array_buffer_indexof[T:Manifest](__arg0: ForgeArrayBuffer[T],__arg1: T): Int
    = __arg0.indexOf(__arg1)
}