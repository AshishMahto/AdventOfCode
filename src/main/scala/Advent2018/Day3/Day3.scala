package Advent2018.Day3

import Shared.Base

object Day3 extends Base with App {

  val reader = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r()

  case class Claim(id: Int, x: Int, y: Int, w: Int, h: Int) {
    def covered: Seq[(Int, Int)] = for {
      xi <- x until (x + w)
      yi <- y until (y + h)
    } yield xi -> yi
  }

  object Claim {
    def from(ln: String): Claim = reader.unapplySeq(ln).get.map(_.toInt) match {
      case List(id, x, y, w, h) => new Claim(id, x, y, w, h)
    }
  }

  val claims = getLines.toSeq map Claim.from
  val claim_count = claims.flatMap(_.covered).freq_map

  claim_count.count(_._2 >= 2).print_part1()

  claims.filter(_.covered.forall(claim_count(_) == 1)).print_part2()
}
