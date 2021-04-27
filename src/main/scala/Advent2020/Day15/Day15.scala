package Advent2020.Day15

import Shared.Base
import scala.collection.mutable.ArrayBuffer

object Day15 extends Base with App {

  val sz = 30000000

  val starting = firstLine.split(",").map(_.toInt)

  def gen(s: Array[Int] = starting, end: Int = 2020) = {
    val ages = ArrayBuffer.fill(sz)(-1)
    s.indices foreach { i => ages(s(i)) = i }
    var x = 0
    var y = 0
    s.length until end - 1 foreach { i =>
      y = ages(x)
      ages(x) = i
      x = if (y >= 0) i - y else 0
    }
    x
  }

  println(gen())

  println(time(gen(end=sz)))
}
