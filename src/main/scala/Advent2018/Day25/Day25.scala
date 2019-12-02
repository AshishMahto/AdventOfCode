package Advent2018.Day25

import Shared.Base

import language.postfixOps

object Day25 extends Base with App {

  type Pt = List[Int]

  implicit class RichPt(p: Pt) {
    def dist(q: Pt): Int = p zip q map Function.tupled(_ - _) map math.abs sum
  }

  val inp: List[Pt] = getLines.toList.map(_.split(',').toList.map(_.toInt))


  def go() = {
    import scala.collection.mutable.Set

    val to_visit = Set.from(inp)
    val constellations = Set[Set[Pt]]()

    while(to_visit.nonEmpty) {
      val constell = Set(to_visit.head)
      to_visit -= to_visit.head
      var added = true

      while (added) {
        val to_add = to_visit.filter(new_p => constell exists (_.dist(new_p) <= 3))
        added = to_add.nonEmpty
        constell ++= to_add
        to_visit --= to_add
      }

      constellations.add(constell)
    }

    constellations
  }

  go().size.print_part1()
}
