package Advent2019.Day9

import Shared.Base

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

object Day9 extends Base with App {
  val reader = """""".r

  val lns = firstLine.split(',').map(_.toLong)


  class IntComp(arr0: ArrayBuffer[Long]) {
    import scala.collection.mutable.Queue
    import scala.collection.View
    import Advent2019.Day9.Day9.Mutable.MOption

    object arr {
      def apply(idx: Long): Long = if (idx < arr0.size) arr0.apply(idx.tryInt) else 0

      def update(idx: Long, v: Long): Unit = {
        val i = idx.tryInt
        if (idx < arr0.size)
          arr0.update(i, v)
        else {
          arr0.sizeHint(i)
          arr0.addAll(ArrayBuffer.fill(i - arr0.size + 1)(0L))
          arr0.update(i, v)
        }
      }
    }

    var relative_base = 0L

    sealed trait PMode extends {
      def apply(i: Long): Long
      def update(i: Long, v: Long): Unit
    }

    case object Position extends PMode {
      def apply(i: Long): Long = arr(i)
      def update(i: Long, v: Long): Unit = arr(i) = v
    }

    case object Value extends PMode {
      def apply(i: Long): Long = i
      def update(i: Long, v: Long): Unit = throw new IllegalArgumentException("Cannot update a value.")
    }

    case object Relative extends PMode {
      def apply(i: Long): Long = arr(i + relative_base)
      def update(i: Long, v: Long): Unit = arr(i + relative_base) = v
    }

    implicit def pmodeFromInt(i: Int): PMode =
      if (i == 0) Position else
      if (i == 1) Value else
      if (i == 2) Relative
      else throw new IllegalArgumentException


    def boolToInt(pred: (Long, Long) => Boolean)(x: Long, y: Long): Int = if (pred(x, y)) 1 else 0

    var out: List[Long] = Nil

    private def defaultInput() = throw new IllegalArgumentException("Input expected, no input given.")
    private def defaultOutput(o: Long): Unit = out ::= o

    val inputs: Queue[Long] = Queue()
    def addInput(x: Long): this.type = {
      inputs addOne x
      this
    }

    def eval1(idx: Long, output: Long => Unit = defaultOutput): Option[Long] = {
      val opcode2 :: opcode1 :: args = arr(idx).digits() reverse_::: List(0, 0, 0)

      implicit val pmode: View[PMode] = args.implicitMap[PMode]

      val List(pm1, pm2) = pmode.toList.take(2)

      def update(fn: (Long, Long) => Long, num_args: Int): Unit = {
        val (pm_tk, pm_dr) = pmode splitAt num_args
        val pm_last = pm_dr.head
        pm_last(arr(idx + num_args + 1)) = pm_tk zip (1 to num_args) map { case (m, i) => m(arr(idx + i)) } reduce fn
      }

      opcode1 * 10 + opcode2 match {
        case 1 =>
          update(_ + _, 2)
          Some(idx + 4)

        case 2 =>
          update(_ * _, 2)
          Some(idx + 4)

        case 3 =>
          val in = inputs.removeHeadOption() getOrElse defaultInput()
          pm1(arr(idx + 1)) = in
          Some(idx + 2)

        case 4 =>
          output(pm1(arr(idx + 1)))
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

        case 9 =>
          relative_base += pm1(arr(idx + 1))
          Some(idx + 2)

        case 99 => None
      }
    }

    @tailrec
    final def eval(idx: Long = 0): List[Long] = {
      eval1(idx) match {
        case None => out
        case Some(i) => eval(i)
      }
    }

    var cur_idx = 0L

    def evalUntilOut(idx: Long = cur_idx): Option[Long] = {
      val has_out = MOption[Long]

      @tailrec
      def go(): Option[Long] = {
        eval1(cur_idx, has_out.set) match {
          case None => None
          case Some(new_idx) =>
            cur_idx = new_idx
            if (has_out.nonEmpty) has_out else go()
        }
      }

      go()
    }
  }

  new IntComp(ArrayBuffer from lns).addInput(1).eval().reverse mkString ", " print_part1

  new IntComp(ArrayBuffer from lns).addInput(2).eval().reverse mkString ", " print_part2

}
