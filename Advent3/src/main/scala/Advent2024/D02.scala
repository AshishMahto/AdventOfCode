package Advent2024

import Shared.D

def remove[T](ls: List[T], index: Int): List[T] = {
  val (before, after) = ls.splitAt(index)
  before ::: after.tail
}

private[this] object D02 extends D {
//  override protected val input =
  """7 6 4 2 1
    |1 2 7 8 9
    |9 7 6 2 1
    |1 3 2 4 5
    |8 6 4 4 1
    |1 3 6 7 9""".stripMargin
  def check(line: List[Int]) = if line.size < 2 then true else {
    val order = line(0) compare line(1)
    def inRange(x: Int) = 1 <= x && x <= 3
    line.zip(line.tail).forall { case (x, y) => (x compare y) == order && inRange(Math.abs(x - y)) }
  }
  def check2(line: List[Int]) = {
    val order = line(0) compare line(1)
    def inRange(x: Int) = 1 <= x && x <= 3
    line.zip(line.tail).zipWithIndex.find { case ((x, y), i) => !((x compare y) == order && inRange(Math.abs(x - y))) } match {
      case None         => true
      case Some(_ -> i) => (i-1 to i+1).exists(j => check(remove(line, j)))
    }
  }
  Input.linesOfNums().count(check).part
  Input.linesOfNums().count(check2).part
}
