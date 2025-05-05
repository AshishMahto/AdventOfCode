package Advent2024

import Shared.D

import scala.collection.View.Updated
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer, Map}
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

private object D06 extends D {
  override val input =
  """....#.....
    |.........#
    |..........
    |..#.......
    |.......#..
    |..........
    |.#..^.....
    |........#.
    |#.........
    |......#...""".stripMargin

  val w = Input.lines.head.length
  val mat = Input.lines.toArray.flatten

  // initial y, x
  val pos0 = mat.zipWithIndex.collectFirst { case ('^', yx) => yx }.get

  // prev -> y, x, next
  val dirs = mutable.Map(
    '^' -> (-w, '>'),
    '>' -> (1, 'v'),
    'v' -> (w, '<'),
    '<' -> (-1, '^')
  )
  def inRange(pos: Int, d: Int) = {
    val pos2 = pos + d
    0 <= pos2 && pos2 < mat.length && (d.abs != 1 || pos/w == pos2/w)
  }

  def go(mat: Array[Char]) = {
    val touched = mutable.Set(pos0)
    val isLoop = mutable.Map.from(dirs.keys.map(_ -> mutable.Set.empty[Int]))
    var pos = pos0
    var dir = '^'
    while dir != '*' && !isLoop(dir)(pos) do
      val (dyx, next) = dirs(dir)
      if !inRange(pos, dyx) then dir = '*'
      else if mat(pos + dyx) == '#' then dir = next
      else
        touched addOne pos
        isLoop(dir) addOne pos
        pos += dyx
    if dir == '*' then touched.size + 1 else -1
  }

  time(go(mat).part)

  val tryAllObstacles = mat.indices.zip(mat).collect { case (i, '.') => mat.clone() thenDo { _(i) = '#' } }

  time(tryAllObstacles.view.map(go).count(-1 == _).part)
}
