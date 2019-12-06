package Advent2019.Day4

import Shared.Base

import scala.language.postfixOps

object Day4 extends Base with App {
  val List(lo, hi) = firstLine.split('-').toList.map(_.toInt)

  def consec_digit(digits: List[Int]) = {
    (digits.init zip digits.tail zipWithIndex) collect { case ((x, y), i) if x == y => i }
  }

  def increasing(digits: List[Int]) = {
    (digits.init zip digits.tail) forall Function.tupled(_ <= _)
  }

  (for {
    i <- lo to hi
    digits = i.digits()
    if consec_digit(digits).nonEmpty && increasing(digits)
  } yield i).length.print_part1()

  def tight_consec(digits: List[Int]): Boolean = {
    val consec_posns = consec_digit(digits)

    def h(consec_pos: Int): Boolean = {
      if (consec_pos == -1) return false
      if (consec_pos == 0) return digits(1) != digits(2)

      val close = digits drop consec_pos - 1
      close(0) != close(1) && close(2) != close.applyOrElse[Int, Int](3, _ => -1)
    }

    consec_posns.exists(h)
  }

  (for {
    i <- lo to hi
    digits = i.digits()
    if tight_consec(digits) && increasing(digits)
  } yield i).length.print_part1()
}
