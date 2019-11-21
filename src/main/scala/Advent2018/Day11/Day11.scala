package Advent2018.Day11

import Advent2018.Base

import scala.collection.mutable

object Day11 extends Base with App {

  val n = 5093

  @inline final
  def power(x: Int, y: Int): Int = {
    val rackId = 10 + x
    (((rackId * y + n) * rackId) / 100) % 10 - 5
  }


  def calcPartialSum(x: Int, y: Int): Int = if (x == 0 || y == 0) 0 else {
    power(x, y) + partialSums(x - 1, y) + partialSums(x, y - 1) - partialSums(x - 1, y - 1)
  }

  val partialSums = mutable.Map[(Int, Int), Int]() toMemoize Function.tupled(calcPartialSum)

  for {
    x <- 1 to 300
    y <- 1 to 300
  } partialSums(x, y)

  @inline final
  def sqrSum(x_raw: Int, y_raw: Int, side_len: Int): Int = {
    val x = x_raw - 1
    val y = y_raw - 1
    partialSums(x + side_len, y + side_len) - partialSums(x, y + side_len) - partialSums(x + side_len, y) +
      partialSums(x, y)
  }

  (for {
    x <- 1 to 298
    y <- 1 to 298
  } yield BestSqr(x, y, 3)).maxBy(_.pwr).print_part1()


  case class BestSqr(x: Int, y: Int, len: Int, pwr: Int)

  object BestSqr {
    def apply(x: Int, y: Int, len: Int): BestSqr = new BestSqr(x, y, len, sqrSum(x, y, len))
  }

  time(for {
    x <- 1 to 300
    y <- 1 to 300
    len <- 1 to (x min y)
  } yield BestSqr(x - len + 1, y - len + 1, len)).maxBy(_.pwr).print_part2()

}
