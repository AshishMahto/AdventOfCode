package Advent2022

import Shared.D

import scala.collection.mutable.ArrayBuffer

private[this] object D05 extends D {
//  override val input =
  """[D]        
    |[N] [C]    
    |[Z] [M] [P]
    | 1   2   3 
    |
    |move 1 from 2 to 1
    |move 3 from 1 to 3
    |move 2 from 2 to 1
    |move 1 from 1 to 2""".stripMargin
  
  val Array(setup, rawMoves) = this.input.split("\n\n")
  val len = setup.split("\n").last.trim.split("\\s+").last.toInt
  val crates =
    val a = ArrayBuffer.fill(len)(List.empty[Char])
    setup.linesIterator.toList.init.reverse foreach { s =>
      0 until len foreach { i =>
        if s(4 * i + 1) != ' ' then
          a(i) ::= s(4 * i + 1)
      }
    }
    a.toVector

  val parseMove = raw"move (\d+) from (\d+) to (\d+)".r
  val moves = rawMoves.split("\n").map { case parseMove(n, l, r) => (n.toInt, l.toInt - 1, r.toInt - 1) }

  moves.foldLeft(crates) { case (a, (n, l, r)) =>
    val (moved, rest_l) = a(l).splitAt(n)
    a.updated(l, rest_l)
     .updated(r, moved reverse_::: a(r))
  }.map(_.head).mkString.part

  moves.foldLeft(crates) { case (a, (n, l, r)) =>
    val (moved, rest_l) = a(l).splitAt(n)
    a.updated(l, rest_l)
     .updated(r, moved ::: a(r))
  }.map(_.head).mkString.part
}
