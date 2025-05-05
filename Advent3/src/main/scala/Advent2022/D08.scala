package Advent2022

import Shared.D

import scala.collection.mutable.ArrayBuffer
import scala.collection.{Factory, MapFactory, mutable}

private object D08 extends D {
  val v = Input.lines.view.map { s => s.map { c => c.toString.toInt } }.toVector
  val seen = v.map { s => ArrayBuffer.from(s.map(_ => 0)) }

  extension [T](ls: IndexedSeq[T])
    def at(i: Int) = if ls.indices contains i then Some(ls(i)) else None

  def tree(index: Vector[Int]) = v.at(index(0)).flatMap(_.at(index(1)))
  def seen_update(index: Vector[Int], t: Int): Unit = seen.at(index(0)).foreach { m => m(index(1)) = t }
  def add(a: Vector[Int], b: Vector[Int]) = a zip b map { _ + _ }

  def look(posn: Vector[Int], vel: Vector[Int]) = Iterable.unfold((posn, -1)) { (cur, height) =>
    tree(cur) flatMap { t =>
      if (t > height) seen_update(cur, 1)
      Some(() -> (add(cur, vel), t max height))
    }
  }

  v.zipWithIndex.map { case (row, x) =>
    look(Vector(x, 0), Vector(0, 1))
    look(Vector(x, row.length - 1), Vector(0, -1))
  }
  v(0).indices map { y =>
    look(Vector(0, y), Vector(1, 0))
    look(Vector(v(0).length - 1, y), Vector(-1, 0))
  }

  seen.map(_.sum).sum.part

  def look2(posn: Vector[Int], vel: Vector[Int]) =
    val height = tree(posn).get
    Iterable.unfold(Option(add(posn, vel), 1)) {
      case Some((cur, count)) => tree(cur) collect {
        case t if t < height => count -> Some(add(cur, vel), count + 1)
        case _ => count -> None
      }
      case None => None
    }.lastOption

  {
    for {
      x <- v.indices
      y <- v(x).indices
      posn = Vector(x, y)
      a <- look2(posn, Vector( 0,  1))
      b <- look2(posn, Vector( 0, -1))
      c <- look2(posn, Vector( 1,  0))
      d <- look2(posn, Vector(-1,  0))
    } yield a * b * c * d
  }.max.part
}
