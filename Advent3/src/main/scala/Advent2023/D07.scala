package Advent2023

import Shared.D

import math.Ordering.Implicits.seqOrdering
import scala.collection.mutable

private[this] object D07 extends D {
//  override protected val input = "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"
  val value = "A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, 2".replaceAll(", ", "").toList.reverse.zipWithIndex.toMap
  val value2 = "A, K, Q, T, 9, 8, 7, 6, 5, 4, 3, 2, J".replaceAll(", ", "").toList.reverse.zipWithIndex.toMap

  val partitions: mutable.Map[Int, List[List[Int]]] = mutable.Map(0 -> List(Nil))
  def getPartition(n: Int, min: Int = 1): List[List[Int]] = partitions.getOrElseUpdate(n, {
    if (n < 0 || n < min) Nil
    else (min max 1).to(n).view.flatMap { i => getPartition(n - i, i).map(i :: _) }.toList
  }).filter(_.headOption.forall(_ >= min))

  def getType5(y: List[Int]) = y.max match {
    case 5                         => 5.0
    case 4                         => 4
    case 3 if y.length == 2        => 3.5
    case 3                         => 3
    case 2 if y.count(_ == 2) == 2 => 2.5
    case 2                         => 2
    case _                         => 1
  }

  val types = mutable.Map[List[Int], Double]()
  def getType(withoutJoker: List[Int]): Double = types.getOrElseUpdate(withoutJoker, {
    if (withoutJoker.isEmpty) 5.0
    else if (withoutJoker.sum == 5) getType5(withoutJoker)
    else withoutJoker.zipWithIndex.map { case (x, i) => getType(withoutJoker.updated(i, x + 1)) }.max
  })

  case class Hand(cards: String, bet: Long) {
    def lambda(cards: String = cards) = cards.view.freqMap.values.toList.sorted
    val order = (getType5(lambda(cards)), cards.map(value.apply))
    lazy val order2 = {
      val noJokers = cards.filter(_ != 'J')
      (getType(lambda(noJokers)), cards.map(value2.apply))
    }
  }

  object Hand {
    def parse(line: String): Hand = {
      val Array(cards, bet) = line.split(' ')
      Hand(cards, bet.toLong)
    }
  }

  Input.lines.map(Hand.parse).sortBy(_.order).zipWithIndex.map { case (hand, i) => hand.bet * (i +1) }.sum.part
  Input.lines.map(Hand.parse).sortBy(_.order2).zipWithIndex.map { case (hand, i) => hand.bet * (i+1) }.sum.part
}
