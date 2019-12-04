package Advent2019.Day3

import Shared.Base

object Day3 extends Base with App {


  val reader = """([RDUL])(\d+)""".r

  type Ln = List[(Int, List[Int])]
  val lns: List[Ln] = {
    getLines.toList
    }.map { ln =>
    ln.split(',').toList.map {
      case reader(dir, v) => v.toInt -> (dir match {
        case "L" => List(0, -1)
        case "R" => List(0, 1)
        case "D" => List(-1, 0)
        case "U" => List(1, 0)
      })
    }
  }


  def follow(ln: Ln) = {
    ln.foldLeft(List(List(0,0))) { case (a, (v, dir)) =>
      (1 to v).foldLeft(a) { case (x :: xs, _) =>
        sum(x, dir) :: x :: xs
      }
    }
  }

  import language.postfixOps


  def sum(p: List[Int], q: List[Int]) = p zip q map Function.tupled(_ + _)
  def dist(p: List[Int], q: List[Int]) = p zip q map Function.tupled(_ - _) map math.abs sum

  val f0 = follow(lns(0)).init
  val len0 = f0.length
  val f1 = follow(lns(1)).init
  val len1 = f1.length

  val intersects = f0 intersect f1

  intersects.print("intersects = ")

  intersects.map(dist(_, List(0,0))).min.print_part1()

  val pts = intersects.map(pt => len0 + len1 - f0.indexOf(pt) - f1.indexOf(pt))

  pts.print("pts = ")

  pts.min.print_part2()
}
