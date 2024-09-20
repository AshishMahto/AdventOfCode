package Advent2023

import Shared.D

import scala.util.matching.Regex

object D03 extends Shared.D {
  val board = Input.lines

  def checkNbrs(mach: Regex.Match, y: Int) = {
    val start = 0.max(mach.start - 1)
    val end = board.length.min(mach.end + 1)
    val symbol = raw"[^.\d]".r.unanchored
    val top = 0 < y && symbol.matches(board(y-1).substring(start, end))
    val bottom = y < (board.size - 1) && symbol.matches(board(y+1).substring(start, end))
    val left = 0 < mach.start && symbol.matches(board(y)(start).toString)
    val right = mach.end < board(y).length && symbol.matches(board(y)(end-1).toString)
    top || bottom || left || right
  }

  def checkNbrs2(mach: Regex.Match, y: Int) = {
    import mach.{start, end}
    for {
      v <- List(Option.when(0 < y)(y-1), Some(y), Option.when(y + 1 < board.size)(y+1)).flatten
      u <- 0.max(start-1) until board.length.min(end+1) if board(v)(u) == '*'
    } yield (v, u) -> (BigInt.long2bigInt(mach.matched.toLong), (y, start, end).hashCode())
  }

  val numbers = for {
    (row, y) <- board.zipWithIndex
    m <- raw"\d+".r.findAllMatchIn(row) if checkNbrs(m, y)
  } yield m.matched

  numbers.map(_.toInt).sum.part

  (for {
    (row, y) <- board.zipWithIndex
    m <- raw"\d+".r.findAllMatchIn(row)
  } yield checkNbrs2(m, y)).flatten.groupMap(_._1)(_._2).values.map(_.distinctBy(_._2).map(_._1))
                           .collect { case List(x, y) => x * y }.sum.toString(10).part

}
