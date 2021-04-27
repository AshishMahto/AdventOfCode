package Shared

import scala.annotation.tailrec
import scala.collection.immutable.{List, Map}
import scala.collection.mutable.ArrayBuffer
import scala.collection.{AbstractView, Factory, IterableOnce, IterableOps, Iterator, StrictOptimizedSeqFactory, mutable}
import scala.language.implicitConversions
import scala.util.Try
import java.lang.System.nanoTime

trait Library {
  var debug_print = true

  def printLines[T](lns: T*): Unit = if (debug_print) lns foreach println

  def time[R](block: => R): R = {
    val t0 = nanoTime()
    try block finally ((nanoTime() - t0) / 1e9).print("Time: ")
  }


  implicit class RichAny[T](x: T) {
    @inline def thenDo(f: T => Unit): T = { f(x) ; x }

    @inline
    private final def p(pfx: String): Unit = println(pfx + x.toString)
    def print(prefix: String = ""): Unit = if (debug_print) p(prefix)
    def print_part1(): Unit = p("Part 1 Answer: ")
    def print_part2(): Unit = p("Part 2 Answer: ")

    def eqv[U >: T](that: U): Boolean = x == that
  }

  implicit class RichTuple2[A, B](p: (A, B)) {
    @inline def combine[T](f: (A, B) => T): T = f(p._1, p._2)
  }

  implicit class RichLong(i: Long) {
    def tryInt: Int = {
      if (Int.MinValue.toLong <= i && i <= Int.MaxValue.toLong)
        i.toInt
      else
        throw new IllegalArgumentException(s"Long $i is too large for int.")
    }

    def digits(base: Int = 10): List[Int] = {
      var b = List[Int]()
      var m = i
      while (m != 0) {
        b ::= (m % base).tryInt
        m /= base
      }
      b
    }
  }

  implicit class RichInt(i: Int) {
    def ifNat[U](f: Int => U): Option[U] = Option.when(i >= 0)(f(i))
    def %%(m: Int): Int = {
      val ret = i % m
      if (ret >= 0) ret else ret + m
    }

    def digits(base: Int = 10): List[Int] = {
      var b = List[Int]()
      var m = i
      while (m != 0) {
        b ::= (m % base)
        m /= base
      }
      b
    }

    @tailrec
    final def gcd(j: Int): Int = if (j == 0) math.abs(i) else math.abs(j).gcd(i % math.abs(j))
  }

  implicit class RichMap[K, V](m: Map[K, V]) {
    def toDefaultMapf[V1 >: V](f: K => V1): Map.WithDefault[K, V1] = new Map.WithDefault[K, V1](m, f)
    def toDefaultMap(d: V): Map.WithDefault[K, V] = new Map.WithDefault[K, V](m, _ => d)

    @inline
    def updatedWith2[V1 >: V](k: K, rf: Option[V] => V1): Map[K, V1] = m + (k -> rf(m get k))

    def inverted[CC](f: Factory[K, CC]): Map[V, CC] = {
      import collection.mutable
      import collection.mutable.Builder

      val acc = mutable.Map.empty[V, Builder[K, CC]]
      for ((value, key) <- m)
        acc.getOrElseUpdate(key, f.newBuilder) += value

      var result = Map.empty[V, CC]
      for ((k, v) <- acc)
        result = result + (k -> v.result())

      result
    }

    def inverted: Map[V, List[K]] = inverted(List)
  }

  implicit class RichDefaultMap[K, V](m: Map.WithDefault[K, V]) {
    import m.defaultValue
    import m.underlying.{removed, updated}

    import Map.WithDefault

    @inline
    final def updatedWith3[V1 >: V](k: K, rf: V => V1): WithDefault[K, V1] =
      new WithDefault(updated(k, rf(m(k))), defaultValue)

    def updatedWith3[V1 >: V](k: K, rf: V => Option[V1])(implicit d: DummyImplicit): WithDefault[K, V1] =
      new WithDefault(rf(m(k)) match {
        case None => removed(k)
        case Some(v) => updated(k, v)
      }, defaultValue)
  }

  implicit class RichMMap[K, V](m: collection.mutable.Map[K, V]) {
    import collection.mutable.Map
    def toDefaultMapf(f: K => V): Map.WithDefault[K, V] = new Map.WithDefault(m, f)
    def toMemoize(f: K => V): Map.WithDefault[K, V] = new mutable.Map.WithDefault(m, f) {
      override def get(k: K): Some[V] = Some(m.get(k) match {
        case None    => f(k) thenDo { m.update(k, _) }
        case Some(v) => v
      })

      override def apply(key: K): V = this.get(key).get
    }
    def toDefaultMap(d: V): Map.WithDefault[K, V] = new Map.WithDefault(m, _ => d)

    // The default .updateWith is very slow
    def updateWith2(k: K, rf: Option[V] => V): V = rf(m get k) thenDo { m += k -> _ }
  }

  implicit class RichDefaultMMap[K, V](m: collection.mutable.Map.WithDefault[K, V]) {
    def updateWith3(k: K, rf: V => V): V = rf(m(k)) thenDo { m += k -> _ }

    /**
      * @param rf If `rf` returns `None`, then the value in the map will be reset to its default.
      */
    def updateWith4(k: K, rf: V => Option[V])(implicit d: DummyImplicit): Option[V] =
      rf(m(k)) thenDo {
        case None => m remove k
        case Some(res) => m addOne k -> res
      }
  }

  implicit class RichIterableOps[A, CC[_], C](s: IterableOps[A, CC, C]) {
    def groupByf[K, V](implicit toKeyVal: A => (K, V)): Map[K, CC[V]] = {
      var cached: (K, V) = null
      def cached_toKeyVal(a: A): (K, V) = {
        cached = toKeyVal(a)
        cached
      }

      s.groupMap(a => cached_toKeyVal(a)._1)(_ => cached._2)
    }

    def implicitMap[B](implicit f: A => B): collection.View[B] = {
      s.view map implicitly
    }
  }

  class UnfoldState[A, S](init: S, f: S => Option[(A, S)]) extends AbstractView[A] {
    var state: S = init
    override def iterator: Iterator[A] = new Iterator[A] {
      //noinspection ConvertNullInitializerToUnderscore
      private[this] var nextResult: Option[(A, S)] = null

      override def hasNext: Boolean = {
        if (nextResult eq null) nextResult = {
          f(state) thenDo (res => if (res eq null) throw new NullPointerException("null during unfold"))
        }
        nextResult.isDefined
      }

      override def next(): A = {
        if (hasNext) {
          val (value, newState) = nextResult.get
          state = newState
          nextResult = null
          value
        } else Iterator.empty.next()
      }
    }
  }

  implicit class RichListObject(l: List.type) {
    def unfoldState[A, S](init: S)(f: S => Option[(A, S)]): (List[A], S) = {
      val us = new UnfoldState(init, f)
      List.from(us) -> us.state
    }
  }

  implicit class RichVectorObject(l: Vector.type) {
    def unfoldState[A, S](init: S)(f: S => Option[(A, S)]): (Vector[A], S) = {
      val us = new UnfoldState(init, f)
      Vector.from(us) -> us.state
    }
  }

  implicit class RichIterableOnce[T](s: IterableOnce[T]) {
    def find_duplicates: Option[((Int, Int), T)] = {
      val so_far = collection.mutable.Map.empty[T, Int]
      val it = s.iterator.zipWithIndex

      Mutable.MOption[((Int, Int), T)] thenDo(dupe => while (dupe.isEmpty && it.hasNext) it.next() thenDo{ case (v, j) =>
        so_far get v match {
          case None => so_far += v -> j
          case Some(i) => dupe.set(i -> j -> v)
        }
      })
    }

    def freq_map: Map.WithDefault[T, Int] = {
      val map = collection.mutable.Map.empty[T, Int].toDefaultMap(0)
      s.iterator foreach { i =>
        map.updateWith3(i, _ + 1)
      }
      Map.from(map.underlying).toDefaultMap(0)
    }

    def cycle: Iterator[T] = Iterator.continually(s).flatten

    /**
      * Returns a 2D sequence where each inner sequence starts with an element that satisfies the predicate,
      * or is the first element in `s`.
      */
    def explode(startSegment: T => Boolean): Iterator[List[T]] = new Iterator[List[T]] {
      private val it = s.iterator

      private val head = Mutable.MOption(it.nextOption())
      private val segment = List.newBuilder[T]

      private def set_init(): Unit = {
        segment.clear()

        while (it.hasNext && { head set it.next() ; !startSegment(head.get) })
          segment += head.get

        if (it.isEmpty) head.clear
      }

      override def hasNext: Boolean = head.isDefined

      override def next(): List[T] = head.map { h =>
        set_init()
        h :: segment.result()
      }.get
    }

    /** Returns a 2D sequence where each inner sequence ends with an element that satisfies the predicate,
      * or is the last element in `s`. */
    def explode_withEnd(endSegment: T => Boolean): Iterator[List[T]] = new Iterator[List[T]] {
      private val it = s.iterator
      private val segment = List.newBuilder[T]

      override def hasNext: Boolean = it.hasNext

      override def next(): List[T] = {
        segment.clear()

        var break = false
        while (it.hasNext && !break) it.next() thenDo { next =>
          segment += next
          if (endSegment(next)) break = true
        }

        segment.result()
      }
    }

    def maxByAll[B](f: T => B)(implicit cmp: Ordering[B]): List[T] = {
      var maxF: B = null.asInstanceOf[B]
      val maxElems = List.newBuilder[T]
      var first = true

      for (elem <- s) {
        val fx = f(elem)
        if (first || cmp.gt(fx, maxF)) {
          maxElems.clear()
          maxElems += elem
          maxF = fx
          first = false
        } else if (cmp.equiv(fx, maxF)) {
          maxElems += elem
        }
      }

      maxElems.result()
    }

    def minByAll[B](f: T => B)(implicit cmp: Ordering[B]): List[T] = {
      var minF: B = null.asInstanceOf[B]
      val minElems = List.newBuilder[T]
      var first = true

      for (elem <- s) {
        val fx = f(elem)
        if (first || cmp.lt(fx, minF)) {
          minElems.clear()
          minElems += elem
          minF = fx
          first = false
        } else if (cmp.equiv(fx, minF)) {
          minElems += elem
        }
      }

      minElems.result()
    }

    @inline
    def maxAll[B >: T](implicit cmp: Ordering[B]): List[T] = maxByAll($conforms[B])(cmp)
    @inline
    def minAll[B >: T](implicit cmp: Ordering[B]): List[T] = minByAll($conforms[B])(cmp)

    def bothMinMax[B >: T](implicit cmp: Ordering[B]): (T, T) = {
      val it = s.iterator
      if (it.isEmpty)
        throw new UnsupportedOperationException("empty.min")
      val init = it.next()
      it.foldLeft(init -> init){ case ((min, max), next) => cmp.min(min, next) -> cmp.max(max, next) }
    }
  }

  object Mutable {
    //noinspection UnitMethodIsParameterless
    abstract class MOption[T](var inner: Option[T]) {
      def set(t: Option[T]): Unit = inner = t

      def set(t: T): Unit = set(Some(t))

      def toOption: Option[T] = inner

      def clear: Unit = set(None)

      def update(f: T => T): Unit = set(inner map f)
      def update(f: T => Option[T])(implicit d: DummyImplicit): Unit = set(inner flatMap f)

      def transform(f: Option[T] => Option[T]): Unit = set(f(inner))
      def transform(f: Option[T] => T)(implicit d: DummyImplicit): Unit = set(f(inner))

      def orElseUpdate(that: => Option[T]): Unit = if (inner.isEmpty) set(that)
      def getOrElseUpdate(that: => T): T = inner getOrElse { that thenDo set }
    }

    object MOption {
      implicit def fromOption[T](m: Option[T]): MOption[T] = MOption(m)
      implicit def toOption[T](m: MOption[T]): Option[T] = m.inner

      def apply[T](inner: Option[T]): MOption[T] = new MOption(inner) {}
      def apply[T](t: T): MOption[T] = new MOption(Some(t)) {}
      def apply[T]: MOption[T] = new MOption[T](None) {}
    }










    /** For most functions that take an index as a parameter, an index `idx` is valid if: <br>
      * - the [[Shared.Library.Mutable.Cycle Cycle]] is `nonEmpty`                    <br>
      * - `idx == 0`
      */
    //noinspection EmptyCheck
    class Cycle[A] private(
                            val rev_front: ArrayBuffer[A],
                            val back     : ArrayBuffer[A],
                          ) extends mutable.AbstractBuffer[A] {

      def this(from: IterableOnce[A]) = {
        this(
          new ArrayBuffer[A](),
          from match {
            case a: ArrayBuffer[A] => a
            case x => ArrayBuffer from x
          })
      }

      def this() = {
        this(new ArrayBuffer[A]())
      }

      protected def this(tkdr: (Iterator[A], Iterator[A])) = {
        this((ArrayBuffer from tkdr._1).reverse, ArrayBuffer from tkdr._2)
      }

      def this(from: IterableOnce[A], index: Int) = {
        this(from.iterator splitAt index)
      }


      override final def knownSize: Int = rev_front.length + back.length

      override def length: Int = rev_front.length + back.length

      override def head: A = back.last

      /** [[Shared.Library.Mutable.Cycle#last() Cycle.last]] is very efficient. */
      override def last: A = rev_front.last

      override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Int = {
        val mid = back.reverseIterator copyToArray (xs, start, len)
        val end = rev_front copyToArray (xs, start + mid, len)
        mid + end
      }

      override def toVector: Vector[A] = super.toVector

      override def iterator: Iterator[A] = back.reverseIterator concat rev_front.iterator

      override def apply(i: Int): A = doAtIdx(i) match { case (arr, j) => arr apply j }

      override def prepend(elem: A): Cycle.this.type = {
        if (back.length == 0)
          back addOne elem
        else
          rev_front addOne elem
        this
      }

      def doAtIdx(idx: Int): (ArrayBuffer[A], Int) = if (idx == 0) (back, 0 max (back.length - 1)) else {
        checkNonempty()

        var i = idx % length
        if (i > 0 &&  i >=     back.length) i -= length
        if (i < 0 && -i > rev_front.length) i += length

        if (i < 0)
          (rev_front, rev_front.length - i)
        else
          (back, back.length - 1 - i)
      }

      override def update(idx: Int, elem: A): Unit = {
        val (arr, i) = doAtIdx(idx)
        arr update (i, elem)
      }


      override def remove(idx: Int, count: Int): Unit = {

        /** Removes elements at indices `(last - count, last]`. Returns `None` if the indices are out of bounds. <br>
          * Note `(last - count, last] == [1 + last - count, 1 + last)` */
        @inline def removeBackwards(arr: ArrayBuffer[A], last: Int, count: Int): Unit =
          arr remove (last + 1 - count, count)

        @inline def trimEnd(arr: ArrayBuffer[A], count: Int): Unit =
          removeBackwards(arr, arr.length - 1, count)

        if (count == 0) ()
        else if (count == length) this.clear()
        else if (count >  length)
          throw new IllegalArgumentException("Can't remove more elements than there are in the cycle.")

        else doAtIdx(idx) match {

          case (`back`, i) => Try {

            removeBackwards(back, i, count)

          } recover { case _ =>
            val remv1_len = i + 1
            Try {

              rev_front remove (0, count - remv1_len) // this might throw, so do it first, before mutating `back`
              back remove (0, remv1_len)

            } recover { case _ =>

              back remove (0, remv1_len)
              back trimEnd count - (rev_front.length + remv1_len)
              rev_front.clear()
            }
          }



          case (`rev_front`, i) => Try {

            rev_front.remove(i, count)

          } recover { case _ =>
            val remv1_len = rev_front.length - i
            Try {

              trimEnd(back, count - remv1_len) // might throw
              rev_front trimEnd remv1_len

            } recover { case _ =>

              rev_front trimEnd remv1_len
              rev_front remove (0, count - (back.length + remv1_len))
              back.clear()
            }
          }
        }
      }

      /** If you remove(0), the cursor will point to the right element. */
      override def remove(idx: Int): A = {
        val (arr, i) = doAtIdx(idx)
        arr remove i thenDo rebalance
      }

      def pop(): A = remove(0)

      /** Don't use this. It isn't polished. */
      override def patchInPlace(from: Int, patch: IterableOnce[A], replaced: Int): Cycle.this.type = {
        println("Warning: This is very preliminary.")
        remove(from, replaced)
        insertAll(from, patch)
        this
      }

      /** Adds an element directly before the cursor, and rotates to the element. */
      override def addOne(elem: A): Cycle.this.type = { back.addOne(elem) ; this }

      override def insertAll(idx: Int, elems: IterableOnce[A]): Unit = doAtIdx(idx) match {
        case (`rev_front`, i) => rev_front insertAll (i, elems)
        case (`back`, i)      => back insertAll (i, ArrayBuffer.from(elems).reverse)
      }

      override def insert(idx: Int, elem: A): Unit = {
        val (arr, i) = doAtIdx(idx)
        arr.insert(i, elem)
      }

      override def clear(): Unit = { rev_front.clear() ; back.clear() }

      override def isEmpty: Boolean = back.isEmpty

      override def trimEnd(n: Int): Unit = {
        var i = n
        if (i < rev_front.length) rev_front.trimEnd(i)
        else {
          i -= rev_front.length
          rev_front.clear()
          if (i < back.length)
            back.remove(0, i)
          else
            back.clear()
        }
      }

      /** Moves `n` elements, from the back of `src` to the back of `dest`, reversed.
        * @param n Must be positive. */
      private def popNTo(src: ArrayBuffer[A],
                         dest: ArrayBuffer[A],
                         n: Int): Unit = {
        dest addAll (src.length - 1 to src.length - n by -1 map src.apply)
        src trimEnd n
      }

      def rebalance(unused: Any): Unit = {
        if (back.isEmpty && rev_front.nonEmpty) {
          back.addAll(rev_front.reverseIterator)
          rev_front.clear()
        }
      }

      class CannotRotateEmpty extends UnsupportedOperationException("Cannot rotate an empty cycle.")

      @inline private def checkNonempty(): Unit = {
        if (back.isEmpty)
          throw new CannotRotateEmpty
      }

      /** Rotates the cycle by `n` to the right. */
      def rotate(n: Int = 1): Unit = {
        checkNonempty()

        var i = n % length
        if (i == 0) return
        else if (i > 0 &&  i >=     back.length) i -= length
        else if (i < 0 && -i > rev_front.length) i += length

        if (0 < i)
          popNTo(back, rev_front, i)
        else if (i < 0)
          popNTo(rev_front, back, -i)
      }

      override def toString(): String = if (isEmpty) "Cycle()" else {
        (rev_front.iterator concat back.reverseIterator)
          .toList.map(_.toString)
          .updated(rev_front.length, "[" + head + "]")
          .mkString("Cycle(", ", ", ")")
      }
    }

    object Cycle extends StrictOptimizedSeqFactory[Cycle] {
      override def empty[A]: Mutable.Cycle[A] = new Cycle[A]()

      override def from[A](source: IterableOnce[A]): Mutable.Cycle[A] =
        new Cycle[A](source)

      override def newBuilder[A]: mutable.Builder[A, Mutable.Cycle[A]] =
        ArrayBuffer.newBuilder[A].mapResult(arr => new Cycle[A](arr))
    }
  }

}
