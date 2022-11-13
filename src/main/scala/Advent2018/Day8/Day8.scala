package Advent2018.Day8

import Shared.Base

import scala.collection.View

object Day8 extends Base with App {

  object Tree {
    val withZeroValue = Tree(0, 0, Vector.empty, Nil)
  }
  case class Tree(
                   num_kids: Int, num_meta: Int,
                   kids: Vector[Tree], meta: List[Int]
                 ) {
    def sum_meta(implicit init: Int = 0): Int = kids.foldLeft(init + meta.sum){ (i, t) => t.sum_meta(i) }

    lazy val value: Int = if (kids.isEmpty) meta.sum else {
      meta.freq_map.map { case (i, freq) =>
          if (kids isDefinedAt (i - 1)) kids(i - 1).value * freq else 0
      }.sum
    }
  }

  def getExample = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

  val inp = getLines.next().split(" ").toList.map(_.toInt)

  def parseTree(inp: List[Int]): (Tree, List[Int]) = {
    val num_kids :: num_meta :: rest = inp
    var i = num_kids

    val (kids, rest2) = Vector.unfoldState(rest) { rest_local =>
      Option.when(i > 0) {
        i -= 1
        parseTree(rest_local)
      }
    }

    val (meta, rest3) = rest2.splitAt(num_meta)
    Tree(num_kids, num_meta, kids, meta) -> rest3
  }

  val (tree, Nil) = parseTree(inp)
  tree.print("tree: ")
  tree.sum_meta.print_part1()

  tree.value.print_part2()
}
