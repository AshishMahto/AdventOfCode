package Advent2022

import Shared.D

private[this] object D06 extends D {
  def go(n: Int) = Input.str.sliding(n).zipWithIndex.collectFirst { case (str, i) if str.toSet.sizeIs == n => i + n }.get
  go(4).part
  go(14).part
}
