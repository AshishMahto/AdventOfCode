package Advent2023

import Shared.D

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.math.Ordering.Implicits.seqOrdering

private[this] object D08 extends D {
//  override protected val input = "RL\n\nAAA = (BBB, CCC)\nBBB = (DDD, EEE)\nCCC = (ZZZ, GGG)\nDDD = (DDD, DDD)\nEEE = (EEE, EEE)\nGGG = (GGG, GGG)\nZZZ = (ZZZ, ZZZ)"
  val lr :: _ :: graph0 = Input.lines
  val graph = graph0.map(_.split(" = ") match {
    case Array(a, uv) =>
      val List(l, r) = "\\w{3}".r.findAllIn(uv).toList
      a -> (l, r)
  }).toMap

  def compress[T](ls: List[T]): List[(T, Int)] = {
    val out = List.newBuilder[(T, Int)]
    var n = 0
    var prev = ls(0)
    ls foreach { x =>
      if (x == prev) n += 1
      else {
        out.addOne(prev -> n)
        n = 1
        prev = x
      }
    }
    out.result()
  }
  val graphs = ListBuffer(graph)
  for (_ <- 1 to 2) graphs.addOne(
    graphs.last.map { case (k, (l, r)) => k -> (
      if (l == "ZZZ") "ZZZ" else graphs.last(l)._1,
      if (r == "ZZZ") "ZZZ" else graphs.last(r)._1,
    )}
  )

  var start = "AAA"
  var steps = 0
  while (start != "ZZZ") {
    start = if (lr(steps % lr.length) == 'L') graph(start)._1 else graph(start)._2
    steps += 1
  }

  steps.part
}
