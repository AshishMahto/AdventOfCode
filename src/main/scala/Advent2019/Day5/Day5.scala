package Advent2019.Day5

import Shared.Base

import scala.collection.View
import scala.collection.mutable.ArrayBuffer

object Day5 extends Base with App {

  sealed trait PMode {
    def apply(i: Int)(implicit arr: ArrayBuffer[Int]): Int
  }
  object PMode {
    case object Address extends PMode {
      def apply(i: Int)(implicit arr: ArrayBuffer[Int]): Int = arr(i)
    }
    case object Value extends PMode {
      def apply(i: Int)(implicit arr: ArrayBuffer[Int]): Int = i
    }

    implicit def fromInt(i: Int): PMode =
      if (i == 0) Address else
      if (i == 1) Value
      else throw new IllegalArgumentException
  }
  import PMode.fromInt

  def boolToInt(pred: (Int, Int) => Boolean)(x: Int, y: Int) = if (pred(x, y)) 1 else 0

  def eval1(idx: Int = 0)(implicit arr: ArrayBuffer[Int], input: Int): Option[Int] = {
    val opcode2 :: opcode1 :: args = arr(idx).digits() reverse_::: List(0,0,0)

    def update(fn: (Int, Int) => Int, num_args: Int)(implicit arr: ArrayBuffer[Int], modes: View[PMode]): Unit =
      arr(arr(idx + num_args + 1)) = modes zip (1 to num_args) map { case (m, i) => m(arr(idx+i)) } reduce fn

    implicit val pmode: View[PMode] = args.implicitMap[PMode]

    val List(pm1, pm2) = pmode.toList.take(2)

    opcode1 * 10 + opcode2 match {
      case 1 =>
        update(_ + _, 2)
        Some(idx + 4)

      case 2 =>
        update(_ * _, 2)
        Some(idx + 4)

      case 3 =>
        arr(arr(idx+1)) = input
        Some(idx + 2)

      case 4 =>
        args(0) apply arr(idx + 1) print "out "
        Some(idx + 2)

      case 5 =>
        Some(if (pm1(arr(idx+1)) == 0) idx+3 else pm2(arr(idx+2)))

      case 6 =>
        Some(if (pm1(arr(idx+1)) == 0) pm2(arr(idx+2)) else idx+3)

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
  def eval(idx: Int = 0)(implicit arr: ArrayBuffer[Int], input: Int): Unit = {
    eval1(idx) match {
      case None =>
      case Some(i) => eval(i)
    }
  }

  val init_inp = firstLine.split(',').map(_.toInt)

  println()
  "".print_part1()
  eval()(ArrayBuffer.from(init_inp), 1)

  println()
  "".print_part2()
  eval()(ArrayBuffer.from(init_inp), 5)
}
