package Advent2019.Day7

import Shared.Base

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Queue}
import scala.language.implicitConversions

object Day7 extends Base with App { Day =>
  import Day.Mutable.MOption

  class IntComp(arr: ArrayBuffer[Int]) {
    import scala.collection.View

    sealed trait PMode extends (Int => Int)
    case object Address extends PMode { def apply(i: Int): Int = arr(i) }
    case object Value extends PMode { def apply(i: Int): Int = i }

    implicit def pmodeFromInt(i: Int): PMode =
      if (i == 0) Address else
      if (i == 1) Value
      else throw new IllegalArgumentException


    def boolToInt(pred: (Int, Int) => Boolean)(x: Int, y: Int): Int = if (pred(x, y)) 1 else 0

    var out: List[Int] = Nil

    private def defaultInput() = throw new IllegalArgumentException("Input expected, no input given.")
    private def defaultOutput(o: Int): Unit = out ::= o

    val inputs: Queue[Int] = Queue()
    def addInput(x: Int): this.type = {
      inputs addOne x
      this
    }

    def eval1(idx: Int, output: Int => Unit = defaultOutput): Option[Int] = {
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
          val in = inputs.removeHeadOption() getOrElse defaultInput()
          arr(arr(idx + 1)) = in
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

        case 99 => None
      }
    }

    @tailrec
    final def eval(idx: Int = 0): List[Int] = {
      eval1(idx) match {
        case None => out
        case Some(i) => eval(i)
      }
    }

    var cur_idx = 0

    def evalUntilOut(idx: Int = cur_idx): Option[Int] = {
      val has_out = MOption[Int]

      @tailrec
      def go(): Option[Int] = {
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


  def part1(p: IndexedSeq[Int]) =
    p.foldLeft(0) { (prev,i) => new IntComp(ArrayBuffer from inp).addInput(i).addInput(prev).eval().head }

  def part2(p: IndexedSeq[Int]) = {
    val comps = p map { in => new IntComp(ArrayBuffer from inp).addInput(in) }

    var init_inp = List(0)
    var cur_comp = 0

    def compsDo1(): Option[Int] = {
      val cur = comps(cur_comp % 5)
      cur.addInput(init_inp.head)
      cur.evalUntilOut() match {
        case Some(out) =>
          cur_comp += 1
          init_inp ::= out
          Some(out)

        case None => None
      }
    }

    var ret: Option[Int] = None
    do {
      ret = compsDo1()
    } while (ret.nonEmpty)

    init_inp.head
  }



  val inp = firstLine.split(',').map(_.toInt)

  val outs = for (p <- (0 to 4).permutations) yield part1(p)

  outs.max.print_part1()

  val outs2 = for (p <- (5 to 9).permutations) yield part2(p)

  outs2.max.print_part2()
}
