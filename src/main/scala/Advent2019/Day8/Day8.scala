package Advent2019.Day8

import Shared.Base

import scala.language.postfixOps

//noinspection EmptyParenMethodAccessedAsParameterless
object Day8 extends Base with App {

  val h = 6
  val w = 25

  val layers = Iterator.unfold(firstLine.iterator) { it =>
    Option.when(it.nonEmpty) {
      (1 to h).map { _ => it take w to IndexedSeq } -> it
    }
  }.toList

  // layers.map(_ mkString "\n").mkString("\n\n-------------------------\n\n").print()

  val minlyr = layers.minBy(_ map (_.count(_ == '0')) sum).flatten
  minlyr.mkString("").print("layer = \n")

  (minlyr count (_ == '1')) * (minlyr count (_ == '2')) print_part1

  val pic = layers.reduceLeft { (toplayer, nextlayer) =>
    toplayer zip nextlayer map { case (topln, nextln) =>
      topln zip nextln map {
        case ('2', nxt) => nxt
        case (top, _) => top
      }
    }
  }

  pic.map(_ map { c => if (c == '1') '#' else ' ' } mkString "").mkString("\n", "\n", "\n").print_part2()
}
