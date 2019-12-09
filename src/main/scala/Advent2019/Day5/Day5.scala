package Advent2019.Day5

import Shared.Base

import scala.collection.mutable.ArrayBuffer

object Day5 extends Base with App {

  class IntComp(arr: ArrayBuffer[Int], input: Int => Int) {
    import scala.collection.View

    private var cur_inp = 0

    sealed trait PMode extends (Int => Int)
    case object Address extends PMode { def apply(i: Int): Int = arr(i) }
    case object Value extends PMode { def apply(i: Int): Int = i }

    implicit def pmodeFromInt(i: Int): PMode =
      if (i == 0) Address else
      if (i == 1) Value
      else throw new IllegalArgumentException


    def boolToInt(pred: (Int, Int) => Boolean)(x: Int, y: Int): Int = if (pred(x, y)) 1 else 0

    var out: List[Int] = Nil

    def eval1(idx: Int = 0): Option[Int] = {
      val opcode2 :: opcode1 :: args = arr(idx).digits() reverse_::: List(0, 0, 0)

      implicit val pmode: View[PMode] = args.implicitMap[PMode]

      val List(pm1, pm2) = pmode.toList.take(2)

      def update(fn: (Int, Int) => Int, num_args: Int): Unit =
        arr(arr(idx + num_args + 1)) = pmode zip (1 to num_args) map { case (m, i) => m(arr(idx + i)) } reduce fn

      opcode1 * 10 + opcode2 match {
        case 1 =>
          update(_ + _, 2)
          Some(idx + 4)

        case 2 =>
          update(_ * _, 2)
          Some(idx + 4)

        case 3 =>
          arr(arr(idx + 1)) = input(cur_inp)
          cur_inp += 1
          Some(idx + 2)

        case 4 =>
          out ::= pm1(arr(idx + 1))
          Some(idx + 2)

        case 5 =>
          Some(if (pm1(arr(idx + 1)) == 0) idx + 3 else pm2(arr(idx + 2)))

        case 6 =>
          Some(if (pm1(arr(idx + 1)) == 0) pm2(arr(idx + 2)) else idx + 3)

        case 7 =>
          update(boolToInt(_ < _), 2)
          Some(idx + 4)

        case 8 =>
          update(boolToInt(_ == _), 2)
          Some(idx + 4)

        case 99 => None
      }
    }

    @scala.annotation.tailrec
    final def apply(idx: Int): List[Int] = {
      eval1(idx) match {
        case None => out
        case Some(i) => apply(i)
      }
    }

    def eval(): List[Int] = apply(0)
  }

  val init_inp = firstLine.split(',').map(_.toInt)

  new IntComp(ArrayBuffer.from(init_inp), List(1)).eval().print_part1()

  new IntComp(ArrayBuffer.from(init_inp), List(5)).eval().print_part2()
}
