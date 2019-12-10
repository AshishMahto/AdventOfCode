package Advent2019.Day10

import Shared.Base

import scala.collection.SortedMap

object Day10 extends Base with App {
  val lns = {
    getLines
    }.toList.map { _.toList }

  case class Slope(x: Int, y: Int, dir: Int)

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
      else if (x == 0) new Slope(0, sgn(y), 0)
      else if (y == 0) new Slope(sgn(x), 0, 0)
      else {
        val gxy = x gcd y
        new Slope(x / gxy, y / gxy, sgn(x))
      }
    }

    /*
    implicit val ordering: Ordering[Slope] = {
      case (Slope(x1, y1, d1), Slope(x2, y2, d2)) =>
        val d_cmp = Ordering.Int.compare(d1, d2)
        if (d_cmp != 0) - d_cmp
        else Ordering.Int.compare(y1 * x2, y2 * x1)
    }

     */
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
      case (x2, y2, '#') => Some(x2 -> y2 -> Slope(x - x2, y - y2))
      case _ => None
    }.flatten.flatten
    case _ => Nil
  }

  (for {
    (ro, y) <- ans.zipWithIndex
    (slope_info, x) <- ro.zipWithIndex
    slopes = slope_info.distinctBy(_._2)
  } yield (x, y, slopes.length, slopes)).maxBy(_._3).print_part1()

}
