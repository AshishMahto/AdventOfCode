package Advent2019.Day10

import Shared.Base

import scala.collection.{SortedMap, mutable}

object Day10 extends Base with App {
  val lns = {
    getLines
    }.toList.map { _.toList }

  case class Slope(x: Int, y: Int, dir: Int) {
    def quad: Int = 0
  }

  object Slope {
    /** Standard rep: (x, y, d)
      * where
      * 0 <= x
      * d = 1 if the point is on the right half, d = -1 if the point is on the left half
      * d = 1 if the point is directly up, d = -1 if the point is directly down
      * if one of the coordinates is 0, the other coordinate is magnitude 1
      */
    def apply(x: Int, y: Int): Slope = {
      def sgn(i: Int) = i / math.abs(i)

      if (x == 0 && y == 0) throw new IllegalArgumentException
      else if (x == 0) new Slope(0, -1, -sgn(y))
      else if (y == 0) new Slope(1, 0, sgn(x))
      else {
        val gxy = x gcd y
        new Slope(math.abs(x / gxy), sgn(x) * y / gxy, sgn(x))
      }
    }

    implicit val ordering: Ordering[Slope] = {
      case (Slope(x1, y1, d1), Slope(x2, y2, d2)) =>
        val d_cmp = Ordering.Int.compare(d1, d2)
        if (d_cmp != 0) -d_cmp
        else Ordering.Int.compare(y1 * x2, y2 * x1) // safer version of `Float.compare( y1 / x1 , y2 / x2 )`
    }

  }

  def gridIter[T](f: (Int, Int, Char) => T): List[List[T]] = {
    for {
      (ro, y) <- lns.zipWithIndex
    } yield for {
      (c, x) <- ro.zipWithIndex
    } yield f(x,y,c)
  }

  val ans = gridIter {
    case (x, y, '#') => gridIter {
      case (`x`, `y`, '#') => None
      case (x2, y2, '#') => Some(Slope(x2 - x, y2 - y) -> (x2, y2))
      case _ => None
    }.flatten.flatten
    case _ => Nil
  }

  val (x0, y0, num_destroyed, destroyed) = (for {
    (ro, y) <- ans.zipWithIndex
    (slope_info, x) <- ro.zipWithIndex
    slopes = slope_info.groupByf.to(mutable.SortedMap)
  } yield (x, y, slopes.size, slopes)).maxBy(_._3)

  num_destroyed.print_part1()


  def dist(x: Int, y: Int) = math.abs(x - x0) + math.abs(y - y0)
  val man_dist = Function.tupled(dist _)

  destroyed.mapValuesInPlace((_, ls) => ls sortBy man_dist)

  var to_destroy = 200 - 1
  var cur_slope = destroyed.head
  while (to_destroy > 0) {
    destroyed.update(cur_slope._1, cur_slope._2.tail)
    to_destroy -= 1

    do {
      cur_slope = destroyed.rangeFrom(cur_slope._1).tail.headOption getOrElse destroyed.head
    } while (cur_slope._2.isEmpty)
  }
  cur_slope.print_part2()



}
