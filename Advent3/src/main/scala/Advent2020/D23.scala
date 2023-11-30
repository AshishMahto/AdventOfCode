package Advent2020

import Shared.D

import scala.math.floorMod
import scala.annotation.targetName
import scala.collection.mutable
import scala.language.implicitConversions

private[this] object D23 extends D {
  def go(v: IndexedSeq[Long], moves: Long) =
    val linked = mutable.Map.from(v.indices map { i => v(i) -> v((i + 1) % v.length) })
    def split(start: Long, n: Long) = 
      val tk -> dr = (0L until n foldLeft (List[Long](), start)) { case ((ls, cur), _) => (cur :: ls, linked(cur)) }
      tk.reverse -> dr
    var cur = v(0)
    0L until moves foreach { _ =>
      val (_ :: init, tail) = split(cur, 4): @unchecked
      linked(cur) = tail
      var search = cur
      var found = Option.empty[Long]
      while found.isEmpty do
        search = floorMod(search - 2, v.length) + 1
        if !init.contains(search) && linked.contains(search) then found = Some(search)
      val after_found = linked(found.get)
      linked(found.get) = init.head
      linked(init.last) = after_found
      cur = tail
    }
    split(1, v.length)._1
  val in = Input.str.trim.map(c => c.toString.toLong)
  go(in, 100).tail.mkString.part
  go(in.concat(Iterable.iterate(10L, 1_000_000 - in.length)(_ + 1)), 10_000_000).take(3).product.part
}
