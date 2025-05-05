package Advent2024

import Shared.D

private object D07 extends D {
//  override val input =
  """190: 10 19
    |3267: 81 40 27
    |83: 17 5
    |156: 15 6
    |7290: 6 8 6 15
    |161011: 16 10 13
    |192: 17 8 14
    |21037: 9 7 18 13
    |292: 11 6 16 20""".stripMargin

  val ops = Set[(Long, Long) => Long](_ * _, _ + _)

  def go(ops: Set[(Long, Long) => Long]) = Input.linesOfNums.collect { case out :: x :: ys =>
    out -> ys.foldLeft(Set(x)) { (possible, y) =>
      possible.flatMap { x => ops.map { _(x, y) }.filter(_ <= out) }
    }.contains(out)
  }.collect { case (out, true) => out } .sum

  go(ops).part

  implicit class Concat(var a: Long):
    def ||(y: Long) =
      var b = y
      while b > 0 do
        a *= 10
        b /= 10
      a + y

  time(go(ops + (_ || _)).part)
}
