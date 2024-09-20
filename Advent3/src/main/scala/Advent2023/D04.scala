package Advent2023

import Shared.D

import scala.util.matching.Regex

object D04 extends Shared.D {
  val cards = Input.lines.collect {
    case s"Card ${id}: ${card} | ${mine}" => (id.trim.toInt, card.trim.split(" +").toList.map(_.toInt), mine.trim.split(" +").toList.map(_.toInt))
  }
  cards.map { case (i, card, mine) =>
    val sz = card.toSet.intersect(mine.toSet).size
    if sz > 0 then 1 << (sz - 1) else 0
  }.sum.part
}
