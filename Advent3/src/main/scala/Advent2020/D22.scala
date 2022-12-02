package Advent2020

import Shared.D

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.util.{Failure, Success, Try}
import scala.language.implicitConversions

object D22 extends D {
  type Deck = Queue[Int]
//  override val input =
  """Player 1:
    |9
    |2
    |6
    |3
    |1
    |
    |Player 2:
    |5
    |8
    |4
    |7
    |10""".stripMargin
  val players = input.split("\n\n").toSeq.map(_.linesIterator.drop(1).map(_.toInt).to[Deck](Queue))
  case class GameOver(players: Seq[Deck]) extends Exception(
    players.flatten.reverse.zipWithIndex.map { case (card, i) => card * (i+1) }.sum.toString
  )
  /** Either an array of 2 players if the game is still going, or an array of 1 player: the winner */
  def round(players: Seq[Deck]) = Try {
    val (p1, p1deck) = players(0).dequeue
    val (p2, p2deck) = players(1).dequeue
    if (p1 > p2)
      Seq(p1deck.enqueueAll(List(p1, p2)), p2deck)
    else
      Seq(p1deck, p2deck.enqueueAll(List(p2, p1)))
  }.recoverWith { case _: NoSuchElementException => Failure(GameOver(players)) }

  var state = Try(players)
  println(s"state = ${state.map(_.mkString(" / "))}")
  while state do
    state = state.flatMap(round)
    println(s"state = ${state.map(_.mkString(" / "))}")
  state.failed.get.getMessage.part

  def game(init: Seq[Deck]): Boolean =
    var ret = false
    var players = Try(init)
    val seen = mutable.Set.empty[Seq[Deck]]
    while !ret && players do players = players flatMap { p =>
      if seen(p) then 
        ret = true
        Success(p)
      else
        seen add p
        round(p)
    }
    ret
}
