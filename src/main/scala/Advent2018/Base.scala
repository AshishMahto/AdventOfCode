package Advent2018

import java.io.{File, FileNotFoundException}

import scala.collection.immutable.Map
import scala.io.Source
import scala.language.implicitConversions

trait Library {

  def printLines[T](lns: T*): Unit = lns foreach println

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    try block finally {
      val t1 = System.nanoTime()
      (t1 - t0).print("Time: ")
    }
  }

  implicit class RichAny[T](x: T) {
    @inline def thenDo(f: T => Unit): T = { f(x) ; x }

    // def thenDo[Unit](f: PartialFunction[T, Unit]): T = try x finally if (f isDefinedAt x) f(x)

    def print(prefix: String = ""): Unit = println(prefix + x.toString)
    def print_part1(): Unit = print("Part 1 Answer: ")
    def print_part2(): Unit = print("Part 2 Answer: ")
  }


  implicit class RichMap[K, V](m: Map[K, V]) {
    def toDefaultMap[V1 >: V](d: V1): Map.WithDefault[K, V1] = new Map.WithDefault[K, V1](m, _ => d)

    @inline
    def updatedWith2[V1 >: V](k: K, rf: Option[V] => V1): Map[K, V1] = m + (k -> rf(m get k))
  }

  implicit class RichDefaultMap[K, V](m: Map.WithDefault[K, V]) {
    import Map.WithDefault
    import m.underlying.updated

    @inline
    def updatedWith3[V1 >: V](k: K, rf: V => V1): WithDefault[K, V1] =
      new WithDefault(updated(k, rf(m(k))), m.defaultValue)

    def updatedWith4[V1 >: V](k: K, rf: V => Option[V1])(implicit d: DummyImplicit): WithDefault[K, V1] =
      new WithDefault(rf(m(k)) match {
        case None => m removed k
        case Some(v) => updated(k, v)
      }, m.defaultValue)
  }

  implicit class RichMMap[K, V](m: collection.mutable.Map[K, V]) {
    import collection.mutable.Map
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


  implicit class RichSeq[T](s: IterableOnce[T]) {

    def find_duplicates: Option[((Int, Int), T)] = {
      val so_far = collection.mutable.Map.empty[T, Int]
      val it = s.iterator.zipWithIndex

      mutable.MOption[((Int, Int), T)] thenDo(dupe => while (dupe.isEmpty && it.hasNext) it.next() thenDo{ case (v, j) =>
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

  }

  object mutable {
    //noinspection UnitMethodIsParameterless
    abstract class MOption[T](var inner: Option[T]) {
      def set(t: Option[T]): Unit = inner = t

      def set(t: T): Unit = set(Some(t))

      def clear: Unit = set(None)

      def update(f: T => T): Unit = set(inner map f)
      def update(f: T => Option[T])(implicit d: DummyImplicit): Unit = set(inner flatMap f)

      def transform(f: Option[T] => Option[T]): Unit = set(f(inner))
      def transform(f: Option[T] => T)(implicit d: DummyImplicit): Unit = set(f(inner))

      def orElseUpdate(that: => Option[T]): Unit = if (inner.isEmpty) set(that)
      def getOrElseUpdate(that: => T): T = inner getOrElse { that thenDo set _ }
    }

    object MOption {
      implicit def toOption[T](m: MOption[T]): Option[T] = m.inner

      def apply[T](inner: Option[T]): MOption[T] = new MOption(inner) {}
      def apply[T](t: T): MOption[T] = new MOption(Some(t)) {}
      def apply[T]: MOption[T] = new MOption[T](None) {}
    }

  }

}

abstract class Base extends AnyRef with Library {

  val debug_path = false

  private val path_to_days = List("src", "main", "scala", "Advent2018").mkString(File.separator)

  val dir = new File(path_to_days)

  if (debug_path) {
    println(
      s"""Your current directory is: '${dir.getCanonicalPath}'
         |If the above directory is not the folder immediately upwards from the DayX folders,
         |Please edit the `Advent2018.Base.path_to_days` to make this the case. Use ".." to move up a folder if needed.
         |""".stripMargin)
  }

  assert(dir.isDirectory)

  private val concrete_class_name = getClass.getSimpleName.init
  private val path_to_input = List(path_to_days, concrete_class_name, "input.txt").mkString(File.separator)

  val input_file = new File(path_to_input)

  if (! input_file.exists()) throw new FileNotFoundException(
      s"Could not find file $path_to_input in directory ${dir.getPath}."
  )

  private val inp = Source.fromFile(input_file)

  /** Returns all of the lines of input from a file named `input2.txt`, as an iterator. <br>
    * Automatically closes the input file once the iterator is empty. <br>
    * Can only be called once.
    * @see [[scala.io.Source#getLines()]]
    */
  def getLines: Iterator[String] = new Iterator[String] {
    val inner: Iterator[String] = inp.getLines()

    // might close the file twice... is that bad?
    override def hasNext: Boolean = inner.hasNext thenDo { x => if (!x) inp.close() }

    override def next(): String = inner.next()
  }
}
