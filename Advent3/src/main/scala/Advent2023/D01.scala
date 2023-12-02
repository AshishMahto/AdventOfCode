package Advent2023

object D01 extends Shared.D {
  val digits = "0123456789"
  val spelled = "one, two, three, four, five, six, seven, eight, nine".split(", ").zipWithIndex.map { case (str, i) => str -> (i + 1).toString }
  Input.lines.map { s =>
    val digs = s.filter(digits.contains)
    s"${digs(0)}${digs(digs.length-1)}".toInt
  }.sum.part

  Input.lines.map { s =>
    val digs = s.indices.flatMap { i =>
      if (digits.contains(s(i))) Some(s"${s(i)}")
      else spelled.collectFirst { case (word, num) if s.drop(i).startsWith(word) => num }
    }
    s"${digs(0)}${digs(digs.length-1)}".toInt
  }.sum.part
}
