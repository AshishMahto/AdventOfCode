package Advent2022

import Shared.D

private[this] object D01 extends D {
  val ls = this.input split "\n\n" map { calories => calories.split("\n").map(_.toInt).sum }
  ls.max.part
  ls.sorted.reverse.slice(0, 3).sum.part
}
