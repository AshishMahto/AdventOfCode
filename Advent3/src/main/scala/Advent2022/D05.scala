package Advent2022

import Shared.D

import scala.collection.mutable.ArrayBuffer

private[this] object D05 extends D {
  val Array(setup, rawMoves) = Input.str.split("\n\n")

  // See: https://www.reddit.com/r/adventofcode/comments/zdqmn2/2022_day_5scala_while_yall_were_complaining_about/
  val crates = setup.split("\n").toList.transpose.collect { case s if s.last.isDigit => s.dropWhile(_.isWhitespace) }
  val moves = rawMoves.split("\n").map { case s"move $n from $l to $r" => (n.toInt, l.toInt - 1, r.toInt - 1) }

  type Concat[T] = (List[T], List[T]) => List[T]
  def go(concat: Concat[Char]) = moves.foldLeft(crates) { case (a, (n, l, r)) =>
    val (moved, rest_l) = a(l) splitAt n
    a.updated(l, rest_l)
     .updated(r, concat(moved, a(r)))
  }.map(_.head).mkString

  go(_ reverse_::: _).part

  go(_ ::: _).part
}
