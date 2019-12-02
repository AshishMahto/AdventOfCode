package Advent2018.Day1

import Shared.Base

object Day1 extends Base with App {
  val num_list = getLines.map(_.toInt).toList

  num_list.sum.print_part1()

  num_list.cycle.scanLeft(0)(_ + _).find_duplicates.print_part2()
}
