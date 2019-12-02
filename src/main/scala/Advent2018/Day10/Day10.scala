package Advent2018.Day10

import Shared.Base

object Day10 extends Base with App {

  val reader = """position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>""".r

  case class R2(x: Int, y: Int) {
    def +(that: R2): R2 = R2(x + that.x, y + that.y)
  }

  case class Pos(pos: R2, vel: R2) {
    def incr: Pos = this.copy(pos = pos + vel)
  }

  case class Board(posns: List[Pos], secs: Int = 0) {
    private val pts = posns.map(_.pos).toSet
    val (top, bot) = pts.map(_.y).bothMinMax
    val (left, right) = pts.map(_.x).bothMinMax

    lazy val incr = Board(posns.map(_.incr), secs + 1)

    /** Not actually the height, but satisfies `ht + 1 = height`. */
    def ht: Int = bot - top

    override def toString: String = {
      (top to bot).map { y =>
        (left to right).map { x =>
          if (pts contains R2(x, y)) '#' else '.'
        }.mkString("")
      }.mkString("\n", "\n", "\n")
    }
  }

  val init_posns = Board(getLines.map {
    case reader(x, y, vx, vy) => Pos(R2(x.toInt, y.toInt), R2(vx.toInt, vy.toInt))
  }.toList)


  var thisBrd = init_posns

  /** We're just assuming that the height will reach a local minimum when the message appears. */
  while (thisBrd.incr.ht < thisBrd.ht) {
    thisBrd = thisBrd.incr
  }

  thisBrd.print_part1()

  thisBrd.secs.print_part2()
}