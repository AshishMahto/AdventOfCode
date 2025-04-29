package Advent2024

import Shared.D

private[this] object D01 extends D {
  val List(l, r) = Input.lines.map(s => raw"\s+".r.split(s).map(_.toInt)).transpose.map(_.sorted)
  l.zip(r).map { case (x, y) => Math.abs(x - y) }.sum.part
  val rFreq = r.freqMap
  l.map { x => x * rFreq.getOrElse(x, 0) }.sum.part
}
