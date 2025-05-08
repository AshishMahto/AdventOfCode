package Advent2024

import Shared.D

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map.from as Dict

private object D11 extends D {
//  override protected val input = """125 17"""
  def split(stone: Long) = {
    var x = stone
    var fst = stone
    var snd = 0L
    var snd10 = 1L
    while x > 9 do
      snd += (fst % 10) * snd10
      snd10 *= 10
      fst /= 10
      x /= 100
    Option.when(x == 0)(List(fst, snd))
  }

  def blink(stone: Long) = if stone == 0L then List(1L) else split(stone) getOrElse List(stone * 2024)

  val stones0 = Input.str.split(' ').map(_.toLong).toList
  var stones = stones0
  1 to 25 foreach { idx =>
    stones = stones.flatMap(blink)
    if idx == 25 then stones.length.part
  }

  val size = 10
  val cache = Array.fill(size * 76)(0L)
  def computeBlinks(blinks: Int)(stone: Long): Long = {
    def rec = blink(stone).map(computeBlinks(blinks - 1)).sum
    if blinks == 0 then 1
    else if stone < size then
      val idx = size * blinks + stone.toInt
      if cache(idx) == 0L then cache(idx) = rec
      cache(idx)
    else rec
  }
  time(stones0.map(computeBlinks(75)).sum).part
}
