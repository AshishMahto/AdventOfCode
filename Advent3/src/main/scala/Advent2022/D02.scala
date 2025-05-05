package Advent2022

import Shared.D

//Exception in thread "main" java.lang.ExceptionInInitializerError
//	at Advent2022.D02.main(D02.scala)
//Caused by: requests.RequestFailedException: Request to https://adventofcode.com/2022/day/2/answer failed with status code 400
//You're posting too much data.

private object D02 extends D {
  val p1 = "ABC"
  val p2 = "XYZ"
//  override val input = 
  """A Y
    |B X
    |C Z""".stripMargin
  this.input.linesIterator.map { s => 
    val List(a, _, x) = s.toList
    p2.indexOf(x) + 1 + 3 * ((p2.indexOf(x) - p1.indexOf(a) + 4) % 3)
  }.sum.part

  this.input.linesIterator.map { s =>
    val List(a, _, x) = s.toList
    3 * p2.indexOf(x) + (p1.indexOf(a) + p2.indexOf(x) + 2) % 3 + 1
  }.toList.sum.part
}
