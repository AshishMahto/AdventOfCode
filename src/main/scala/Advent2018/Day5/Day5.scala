package Advent2018.Day5

import Advent2018.Base

import scala.annotation.tailrec

object Day5 extends Base with App {
  val str = getLines.toList.head.toList

  def annihilate(a: Char, b: Char): Boolean = a.isLower != b.isLower && a.toLower == b.toLower

  @tailrec
  def annihilate(polymer: List[Char], init: List[Char] = Nil): List[Char] = polymer match {
    case Nil | List(_) => init reverse_::: polymer
    case x :: y :: rest if annihilate(x, y) => init match {
      case Nil => annihilate(rest)
      case h :: t => annihilate(h :: rest, t)
    }
    case h :: t => annihilate(t, h :: init)
  }

  annihilate(str).length.print_part1()

  str.map(_.toLower).distinct.map(c => annihilate(str.filter(c != _.toLower)).length).min.print_part2()

}
