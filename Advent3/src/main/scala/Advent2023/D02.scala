package Advent2023

import Shared.D

object D02 extends Shared.D {

  case class Marbles(i: Int, colour: String)

  object Marbles:
    def parse(unparsed: String) =
      val s"$num $colour" = unparsed: @unchecked
      Marbles(num.toInt, colour)

  case class Round(marbles: List[Marbles]):
    lazy val colourMap = marbles.groupMapReduce(_.colour)(_.i)(_ + _)
    def <=(that: Round): Boolean = colourMap.forall { case (clr, i) => i <= that.colourMap.getOrElse(clr, 0) }
    def max(that: Round): Round = Round.fromMap((colourMap.toSeq ++ that.colourMap).groupMapReduce(_._1)(_._2)(_ max _))
    def power = marbles.map(_.i).product

  object Round:
    def parse(unparsed: String) = Round(unparsed.split(", ").map(Marbles.parse).toList)
    def fromMap(colourMap: Map[String, Int]) = Round(colourMap.map { case (clr, i) => Marbles(i, clr) }.toList)
    val total = Round.parse("12 red, 13 green, 14 blue")

  val games = Input.lines.map { ln =>
    val Array(num, game) = ln.split(": ")
    num.split(' ')(1).toInt -> game.split("; ").toList.map(Round.parse)
  }

  games.collect { case (index, game) if game.forall(round => round <= Round.total) => index }.sum.part
  games.map { case (i, game) => game.reduce(_ max _).power }.sum.part
}
