package Advent2018.Day2

import Advent2018.Base

object Day2 extends Base with App {

  def duplicate_counts[T](ls: List[T], s: Set[Int] = Set.empty): Set[Int] = ls match {
    case Nil => s
    case h :: t =>
      val (eqs, not_eqs) = ls.partition(_ == h)
      duplicate_counts(not_eqs, s + eqs.length)
  }

  val all_words = getLines.toSeq
  val word_freqs = all_words.flatMap(s => duplicate_counts(s.toList)).freq_map

  word_freqs.print("freq map: ")

  (word_freqs(2) * word_freqs(3)).print_part1()

  val all_word_diffs = for {
    v <- all_words.iterator
    w <- all_words.iterator
    if v != w
  } yield v zip w filter { case (i, j) => i == j } map (_._1)

  all_word_diffs.maxBy(_.length).mkString("").print_part2()
}
