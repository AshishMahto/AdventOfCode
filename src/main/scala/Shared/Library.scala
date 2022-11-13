package Shared

import scala.annotation.tailrec
import scala.collection.immutable.{List, Map}
import scala.collection.{AbstractView, Factory, IterableOnce, IterableOps, Iterator, mutable}
import scala.language.implicitConversions
import java.lang.System.nanoTime

//noinspection ScalaUnusedSymbol
trait Library {
  var debug_print = true

  def printLines[T](lns: T*): Unit = if (debug_print) lns foreach println

  def time[R](block: => R): R = {
    val t0 = nanoTime()
    try block finally ((nanoTime() - t0) / 1e9).print("Time: ")
  }


  implicit class RichAny[T](x: T) {
    @inline def thenDo(f: T => Unit): T = { f(x); x }

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
        case None    => removed(k)
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
        case None      => m remove k
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

      Mutable.MOption[((Int, Int), T)] thenDo (dupe => while (dupe.isEmpty && it.hasNext) it.next() thenDo { case (v, j) =>
        so_far get v match {
          case None    => so_far += v -> j
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

        while (it.hasNext && { head set it.next(); !startSegment(head.get) })
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

    @inline def maxAll(implicit cmp: Ordering[T]): List[T] = maxByAll(x => x)
    @inline def minAll(implicit cmp: Ordering[T]): List[T] = minByAll(x => x)

    def bothMinMax[B >: T](implicit cmp: Ordering[B]): (T, T) = {
      val it = s.iterator
      if (it.isEmpty)
        throw new UnsupportedOperationException("empty.min")
      val init = it.next()
      it.foldLeft(init -> init) { case ((min, max), next) => cmp.min(min, next) -> cmp.max(max, next) }
    }
  }

  object Mutable {
    class MOption[T] private (private var inner: Option[T]) {
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
      def apply[T](inner: Option[T]): MOption[T] = new MOption(inner)
      def apply[T](t: T): MOption[T] = new MOption(Option(t))
      def apply[T]: MOption[T] = new MOption[T](None)
    }
  }
}
