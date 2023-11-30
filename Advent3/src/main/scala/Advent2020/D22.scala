package Advent2020

import Shared.D

import java.util.NoSuchElementException
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.{Factory, mutable}
import scala.util.{Failure, Success, Try}
import scala.language.implicitConversions

object D22 extends D {
  type Deck = Queue[Long]
  type Decks = Seq[Deck]
  val Deck = Queue
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
  val players = Input.str.split("\n\n").toSeq.map(_.linesIterator.drop(1).map(_.toLong).to(Deck))

  case class GameOver(players: Decks) extends Exception(
    players.flatten.reverse.zipWithIndex.map { case (card, i) => card * (i+1) }.sum.toString
  ) {
    def p1Win = players(0).nonEmpty
    override def toString = s"GameOver($players, winner = ${if (p1Win) 1 else 2})"
  }

  object Deque {
    def unapply(queue: Deck) = queue.dequeueOption
  }

  @tailrec
  def game1(players: Decks): GameOver = players match {
    case Seq(Deque(p1, p1deck), Deque(p2, p2deck)) =>
      if (p1 > p2) game1(Seq(p1deck enqueueAll Seq(p1, p2), p2deck))
      else game1(Seq(p1deck, p2deck enqueueAll Seq(p2, p1)))
    case _ => GameOver(players)
  }

  game1(players).getMessage.part

  val default = Seq(Deck(1L), Deck())
  val seen = mutable.Map[List[Decks], GameOver]()

  case class Game()

  /** Compute a GameOver.
    * @param games The stack of games; first is the finished game, second is the next game.
    * @param gameOver The result of the first (now finished) game.
    * @return The stack of games with the first removed and the second edited according to winner.
    * @throws GameOver If the stack of games is all done.
    */
  def nextGame(games: List[Decks], gameOver: GameOver): List[Decks] = games match {
    case doneGame :: Seq(Deque(p1, p1deck), Deque(p2, p2deck)) :: restGames =>
      if (gameOver.p1Win) Seq(p1deck enqueueAll Seq(p1, p2), p2deck) :: restGames
      else                Seq(p1deck, p2deck enqueueAll Seq(p2, p1)) :: restGames
    case doneGame :: Nil => throw gameOver
    case _ => throw new IllegalArgumentException(s"could not match games = $games")
  }

  /**
    * @param games A stack of games remaining to play.
    * @throws GameOver when the game is over.
    */
  @tailrec
  def go(games: List[Decks]): Nothing = {
    val players :: restGames = games: @unchecked
    if (seen.contains(games)) go(nextGame(games, seen(games)))
    else if (players.exists(_.isEmpty)) go(nextGame(games, GameOver(players)))
    else {
      seen(games) = GameOver(default)
      val Seq(Deque(p1, p1deck), Deque(p2, p2deck)) = players: @unchecked
//      println(s"${"  " * restGames.length}round = ${players(0)}\n${"  " * restGames.length}        ${players(1)}".stripMargin)
      if (p1 <= p1deck.length && p2 <= p2deck.length)
        go(Seq(p1deck take p1.toInt, p2deck take p2.toInt) :: games)
      else if (p1 > p2) {
        go(Seq(p1deck enqueueAll Seq(p1, p2), p2deck) :: restGames)
      } else {
        go(Seq(p1deck, p2deck enqueueAll Seq(p2, p1)) :: restGames)
      }
    }
  }

  Try(go(List(players))).failed.get.getMessage.part
}
