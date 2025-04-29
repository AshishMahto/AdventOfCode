package Advent2024

import Shared.D

private[this] object D03 extends D {
//  override protected val input =
  """xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))""".stripMargin
  raw"mul\((\d{1,3}),(\d{1,3})\)".r.findAllMatchIn(Input.str).toList.map { m =>
    val a = m.group(1).toInt
    val b = m.group(2).toInt
    a * b
  }.sum.part

  val matches = raw"mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)".r.findAllMatchIn(Input.str)
  Iterator.unfold(true) { enabled =>
    Option.when(matches.hasNext) {
      val m = matches.next()
      val op = m.matched.split("\\(")(0)
      if op == "mul" then (if enabled then m.group(1).toInt * m.group(2).toInt else 0, enabled)
      else if op == "don't" then (0, false)
      else (0, true)
    }
  }.sum.part
}
