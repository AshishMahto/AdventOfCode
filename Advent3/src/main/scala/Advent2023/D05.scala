package Advent2023

import Shared.D

import scala.collection.{GenIterable, mutable}

private[this] object D05 extends D {

//  override protected val input = "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4"

  val seeds0 :: maps0 = Input.str.split("\n\n.*\n").toList: @unchecked
  val seeds = seeds0.split(' ').toList.tail.map(_.toLong).thenDo(_.pr("seeds = "))

  type LinearMap = mutable.LinkedHashMap[Long, Long]
  val LinearMap = mutable.LinkedHashMap

  case class Range(in: Long, len: Long, out: Long) {
    def asMap: LinearMap = Iterator.tabulate(len.toInt)(i => (in + i) -> (out + i)).to(LinearMap)
    def at(x: Long): Option[Long] = Option.when(in <= x && x < in + len)(out + x - in)
    override def toString: String = s"[$in, ${in + len}) -> [$out, ${out + len})"
  }

  extension (ranges: List[Range]) {
    def at(x: Long): Long = ranges.flatMap(_.at(x)).headOption.getOrElse(x)
    def asString: String = ranges.mkString("\t", "\n\t", "\n")
  }

  object Range {
    def apply(str: String): Range = {
      val List(out, in, len) = str.split(' ').map(_.toLong).toList
      Range(in, len, out)
    }
    def stack(ranges: List[Range]): LinearMap = ranges.flatMap(_.asMap).to(LinearMap)
    def compose(prev: Map[Long, Long], next: Map[Long, Long]): Map[Long, Long] = {
      next ++ prev.map { case (k, v) => k -> next.getOrElse(v, v) }
    }
  }

  case class Seeds(x: Long, y: Long, level: String = "") {
    def transform(range: Range): List[Seeds] = if (level.isEmpty) {
      val (p, q) = (range.in, range.in + range.len)
      val List(a, b, c, d) = List(p, q, x, y).sorted
      List(
        Seeds(a, b),
        Seeds(b, c),
        Seeds(c, d),
      ).flatMap { case Seeds(u, v, _) =>
        // if it's inside the seeds
        if (x <= u && v <= y) Some((range.at(u), range.at(v - 1)) match {
          case (Some(r), Some(s)) => Seeds(r, s + 1, "+")
          case _                  => Seeds(u, v)
        })
        else None
      }
    } else List(this)
    override def toString: String = s"[$x, $y)"
  }

  extension (seeds: List[Seeds]) {
    def resetLevels = seeds.map(_.copy(level = ""))
    def transform(range: Range): List[Seeds] = seeds.flatMap(_ transform range)
    def transform(ranges: List[Range]): List[Seeds] = ranges.foldLeft(seeds) { _ transform _ }.simplify.thenDo { s => println(s"seeds = $seeds;\nrange = ${ranges.sortBy(_.in)}\n") }
    def simplify: List[Seeds] = seeds.filter(s => s.x < s.y).sortBy(_.x).resetLevels
  }

  val maps = maps0.map(m => m.split('\n').toList.map(Range.apply)).thenDo(_.pr("maps = "))
  seeds.map(maps(0).at).pr("seeds -> soil = ")
  maps.foldLeft(seeds) { case (seeds, table) => seeds map table.at }.min.thenDo(_.pr("seeds -> location = ")).part

  val seeds2 = seeds.grouped(2).toList.map { case List(x, l) => Seeds(x, x + l) }.simplify

  println()
  maps.foldLeft(seeds2) { _ transform _ }
      .thenDo { s => println(s"last = $s") }.map(_.x).min.thenDo(_.pr("min = ")).part
}
