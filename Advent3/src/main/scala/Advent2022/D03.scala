package Advent2022

import Shared.D

private[this] object D03 extends D {
  Input.lines.map { s => 
    val (l, r) = s.splitAt(s.length >> 1)
    val c = l.toSet.intersect(r.toSet).head
    c.toInt - (if (c.isLower) 'a'.toInt - 1 else 'A'.toInt - 27) 
  }.sum.part

  Input.lines.grouped(3).map { ls => 
    val c = ls.map(_.toSet).reduce(_ intersect _).head
    c.toInt - (if (c.isLower) 'a'.toInt - 1 else 'A'.toInt - 27) 
  }.sum.part
}
