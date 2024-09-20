package Advent2020

import Shared.D

import scala.collection.mutable.ArrayBuffer

private[this] object D24 extends D {
  case class Pos(y: Int, x: Int) {
    def +(pos: Pos) = Pos(y + pos.y, x + pos.x)
  }

  val dirs = Map(
     "e" -> Pos(0, 2),
     "w" -> Pos(0, -2),
    "ne" -> Pos(1, 1),
    "nw" -> Pos(1, -1),
    "se" -> Pos(-1, 1),
    "sw" -> Pos(-1, -1),
  )

  def parse(s: String): ArrayBuffer[Pos] = ArrayBuffer[Pos]().thenDo { a =>
    var i = 0
    while (i < s.length) {
      a += dirs.getOrElse(s.slice(i, i+1), {
        i += 1
        dirs(s.slice(i-1, i+1))
      })
      i += 1
    }
  }

  val hexs = Input.lines.map(parse).map { line =>
    line.reduce(_ + _)
  }

  type Board = Set[Pos]

  val board0 = hexs.groupMapReduce(x => x)(_ => 1)(_ ^ _).filter { _._2 == 1 }.keys.toSet
  board0.size.part

  extension (board: Board)
    def toggle(pos: Pos): Board = if (board(pos)) board - pos else board + pos
    def neighbours(pos: Pos) = dirs.values.map { dir => pos + dir }
    def b_nbrs(pos: Pos) = board.neighbours(pos).count(board.contains)
    def black_stays_black(p: Pos) = board(p) && { val x = board.b_nbrs(p); x == 1 || x == 2 }
    def white_turns_black(p: Pos) = !board(p) && board.b_nbrs(p) == 2

  def nextDay(board: Board) = board
    .flatMap(board.neighbours)
    .filter { p => board.black_stays_black(p) || board.white_turns_black(p) }

  val days = Iterator.unfold(board0)(board => Some(board.size, nextDay(board)))
  days.take(101).toSeq.last.part
}
