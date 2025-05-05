package Advent2024

import Shared.D

private object D08 extends D {
//  override val input =
  """............
    |........0...
    |.....0......
    |.......0....
    |....0.......
    |......A.....
    |............
    |............
    |........A...
    |.........A..
    |............
    |............""".stripMargin

  class Board(val matrix: Array[String]) {
    val h = matrix.length
    val w = matrix(0).length

    case class Pos(y: Int, x: Int):
      def +(d: Pos) = Pos(y + d.y, x + d.x)
      def -(d: Pos) = Pos(y - d.y, x - d.x)
      def reduce =
        val g = gcd(y, x)
        if g == 0 then this else Pos(y / g, x / g)
      def %(d: Pos) =
        val k = Math.max(y / d.y, x / d.x)
        Pos(y - k * d.y, x - k * d.x)
      def isValid = 0 <= y && y < h && 0 <= x && x < w

    def posIter = new IndexedSeq[(Pos, Char)]:
      def length = h * w
      def apply(i: Int) =
        val y = i / w
        val x = i % w
        Pos(y, x) -> matrix(y)(x)
  }

  val board = new Board(Input.str.linesIterator.toArray) {
    val towers = posIter.toList.filter(_._2 != '.').groupMap(_._2)(_._1)

    towers.flatMap { (c, posns) =>
      posns.pairs.flatMap { case (left, right) =>
        val d = right - left
        List(left - d, right + d).filter(_.isValid)
      }
    }.toSet.thenDo(_.size.part)

    val antinodes2 = towers.flatMap { (c, posns) =>
      posns.pairs.flatMap { case (left, right) =>
        val d = (right - left).reduce
        val init = left % d
        Iterator.iterate(init)(_ + d).dropWhile(!_.isValid).takeWhile(_.isValid)
      }
    }.toSet.thenDo(_.size.part)
  }
}
