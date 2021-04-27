package Advent2020.Day15

import Shared.Base

object Day15 extends Base with App {

  val sz = 30000000

  val starting = firstLine.split(",").map(_.toInt)

  def gen(s: Array[Int] = starting, end: Int = 2020) = {
    val ages = Array.fill(sz)(0)
    s.indices foreach { i => ages(s(i)) = i }
    var x = s.length
    ages(0) = x
    var y = 0
    s.length + 1 until end - 1 foreach { i =>
      y = ages(x)
      ages(x) = i
      x = if (y == 0) 0 else i - y
    }
    x
  }

  println(gen())

  time(println(gen(end=sz)))
}
