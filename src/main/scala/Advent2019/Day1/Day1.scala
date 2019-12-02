package Advent2019.Day1

import Shared.Base

object Day1 extends Base with App {

  val inp = 3


  val lns = getLines.toList.map(_.toInt)

  def f(x: Int) = x / 3 - 2

  lns.map(f).sum.print_part1()

  def forOne(i: Int) = Iterator.unfold(i) { i =>
    Option.when(f(i) > 0)(f(i) -> f(i))
  }

  println(forOne(1969).mkString(", "))

  lns.map(forOne(_).sum).sum.print_part2()
}
