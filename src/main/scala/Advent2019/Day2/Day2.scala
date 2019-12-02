package Advent2019.Day2

import Shared.Base

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

object Day2 extends Base with App {
  val init_inp = firstLine.split(',')

  def mk_inp: ArrayBuffer[Int] = init_inp.to(ArrayBuffer).map(_.toInt)

  @scala.annotation.tailrec
  def eval(idx: Int = 0)(implicit inp: ArrayBuffer[Int]): Boolean = {
    inp.apply(idx) match {
      case 1 =>
        inp.update(inp(idx + 3), inp(inp(idx + 1)) + inp(inp(idx + 2)))
        eval(idx + 4)
      case 2 =>
        inp.update(inp(idx + 3), inp(inp(idx + 1)) * inp(inp(idx + 2)))
        eval(idx + 4)
      case 99 => true
    }
  }

  def go(noun: Int = 12, verb: Int = 2) = {
    implicit val inp: ArrayBuffer[Int] = mk_inp

    inp.update(1, noun)
    inp.update(2, verb)
    eval()
    inp.head
  }

  go().print_part1()

  for {
    noun <- 0 to init_inp.length
    verb <- 0 to init_inp.length
  } Try(go(noun, verb)).map(head => if (head == 19690720) (noun * 100 + verb).print_part2())

}
