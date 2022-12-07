package Advent2022

import Shared.D

private[this] object D06 extends D {
  Input.str.sliding(4).zipWithIndex.collectFirst { case (str, i) if str.toSet.sizeIs == 4 => i + 4 }.get.part
  Input.str.sliding(14).zipWithIndex.collectFirst { case (str, i) if str.toSet.sizeIs == 14 => i + 14 }.get.part
}
