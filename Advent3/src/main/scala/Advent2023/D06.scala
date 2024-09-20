package Advent2023

import Shared.D

private[this] object D06 extends D {
//  override protected val input = "Time:      7  15   30\nDistance:  9  40  200"

  val List(times, dists) = Input.lines.map(_.split("\\s+").toList.tail.map(_.toLong))
  val races = times zip dists

  extension (time: Long) {
    def solve(dist: Long) = {
      val low = 0.5 * (time - Math.sqrt(time * time - 4 * dist))
      val high = 0.5 * (time + Math.sqrt(time * time - 4 * dist))
      (time, dist, high, low, ((high - 1).ceil - (low + 1).floor + 1).round)
    }
  }

  races.map(Function.tupled(_ solve _)).thenDo(_.pr("ans = ")).map(_._5).product.part
  val List(time, dist) = Input.lines.map(_.split(':')(1).replaceAll(" ", "").toLong).thenDo(_ pr "inp2 = ")
  time.solve(dist).thenDo(println)._5.part
}
