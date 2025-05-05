package Advent2022

import Shared.D
import scala.annotation.targetName
import scala.util.control.NonLocalReturns.*

private object D09 extends D {
//  override protected val input = 
  """R 4
    |U 4
    |L 3
    |D 1
    |R 4
    |D 1
    |L 5
    |R 2""".stripMargin

  type P = List[Int]
  val P = List

  def dist(s: P, t: P) = s.zip(t).map { case (x, y) => Math.abs(x - y) }.max
  def add(s: P, t: P) = s.zip(t).map(_ + _)
  val vels = Map("R" -> P(1,0), "L" -> P(-1, 0), "U" -> P(0, -1), "D" -> P(0, 1))
  case class State private (head: P, tails: List[P]):
    def tail = tails.head
    def move(head: P = head) =
      if dist(head, tail) <= 1 then State(head, tail :: tails)
      else State(head, this.head :: tails)
    def move2(head: P = head): State =
      val (tails9, visited) = tails.splitAt(9)
      val _ :: newTails9 = tails9.foldLeft(List(head)) { case (ls, tail) =>
        val newTail = if dist(ls.head, tail) <= 1 then tail else ls.head.zip(tail).map { case (a, b) => b + (a - b).sign }
        newTail :: ls
      }.reverse: @unchecked
      State(head, newTails9 ::: newTails9.last :: visited)

  object State:
    @targetName("apply1") def apply(head: P = P(0, 0), tail: P = P(0, 0)) = new State(head, List.fill(10)(tail))

  def go(move: (State, P) => State): State = returning {
    Input.lines.foldLeft(State()) {
      case (state, s"$dir $n") => (1 to n.toInt foldLeft state) { case (st, _) => move(st, add(st.head, vels(dir))) }
      case _ => throw IllegalArgumentException()
    }
  }

  go(_ `move` _).tails.toSet.size.part
  go(_ `move2` _).tails.drop(8).toSet.size.part
}
