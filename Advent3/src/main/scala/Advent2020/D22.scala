package Advent2020

import Shared.D

import java.util.NoSuchElementException
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.{Factory, mutable}
import scala.util.{Failure, Success, Try}
import scala.language.implicitConversions

object D22 extends D {
  type Deck = Queue[Int]
  type Decks = Seq[Deck]
  val Deck = Queue
  val players = Input.str.split("\n\n").toSeq.map(_.linesIterator.drop(1).map(_.toInt).to(Deck))

  case class GameOver(players: Decks) extends Exception(
    players.flatten.reverse.zipWithIndex.map { case (card, i) => card * (i+1) }.sum.toString
  ) {
    def p1Win = players(0).nonEmpty
    override def toString = s"GameOver($players, winner = ${if (p1Win) 1 else 2})"
  }

  object Deque:
    def unapply(queue: Deck) = queue.dequeueOption

  @tailrec
  def game1(players: Decks): GameOver = players match {
    case Seq(Deque(p1, p1deck), Deque(p2, p2deck)) =>
      if (p1 > p2) game1(Seq(p1deck enqueueAll Seq(p1, p2), p2deck))
      else game1(Seq(p1deck, p2deck enqueueAll Seq(p2, p1)))
    case _ => GameOver(players)
  }

  game1(players).getMessage.part

  val default = GameOver(Seq(Deck(1), Deck()))
  type Key = (Decks, Int)
  val seen = mutable.Set[Key]()
  @inline
  def key(decks: List[Decks]): Key = (decks.head, decks.length) // takes a long time to hash List[Decks]

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
    if (seen(key(games))) go(nextGame(games, default))
    else if (players.exists(_.isEmpty)) go(nextGame(games, GameOver(players)))
    else {
      seen += key(games)
      val Seq(Deque(p1, p1deck), Deque(p2, p2deck)) = players: @unchecked
      if (p1 <= p1deck.size && p2 <= p2deck.size) go(Seq(p1deck take p1, p2deck take p2) :: games)
      else if (p1 > p2)                           go(Seq(p1deck enqueueAll Seq(p1, p2), p2deck) :: restGames)
      else                                        go(Seq(p1deck, p2deck enqueueAll Seq(p2, p1)) :: restGames)
    }
  }

  time {
    Try(go(List(players))).failed.get.getMessage.part
  }
}
