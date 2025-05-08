package Advent2024

import Shared.D

import scala.collection.mutable.Map.{from => Dict}

private object D10 extends D {
  class Board(val matrix: Array[String]) {
    val h = matrix.length
    val w = matrix(0).length

    case class Pos(y: Int, x: Int):
      def +(d: Pos) = Pos(y + d.y, x + d.x)
      def isValid = 0 <= y && y < h && 0 <= x && x < w
      def getCell = Option.when(isValid)(matrix(y)(x))
  }

  new Board(Input.str split '\n') {
    val posns = matrix.toList.zipWithIndex.flatMap { (row, y) =>
      row.zipWithIndex collect { case (c, x) => Pos(y, x) -> c }
    }.groupMap(_._2)(_._1)

    val nbrDirs = List(Pos(0, 1), Pos(1, 0), Pos(0, -1), Pos(-1, 0))
    val scores = Dict(posns('9') map { pos => pos -> Set(pos) })
    val ratings = Dict(posns('9') map { _ -> 1 })
    '8'.toInt.to('0', -1) foreach { level =>
      posns(level.toChar) foreach { pos =>
        val validNbrs = nbrDirs.map(pos + _).filter(p => p.getCell contains level + 1)
        val score = validNbrs flatMap scores.get
        val rating = validNbrs flatMap ratings.get
        scores(pos) = score.foldLeft(Set())(_ union _)
        ratings(pos) = rating.sum
      }
    }
    posns('0').map { pos => scores.get(pos).fold(0)(_.size) }.sum.part
    posns('0').map { pos => ratings.getOrElse(pos, 0) }.sum.part
  }
}
