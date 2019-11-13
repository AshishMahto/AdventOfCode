package Advent2018.Day6

import Advent2018.Base
import fansi.{Attr, Color}

import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.Try

object Day6 extends Base with App {

  val lines = getLines.toList

  val pts = lines.zipWithIndex.map { case (ln, i) => ln.split(", ") match {
    case Array(x: String, y: String) => Pt(i, Pos(y.toInt, x.toInt))
  }}

  object old_pt {
    private val (ys, xs) = pts.map(p => p.pos.y -> p.pos.x).unzip

    val ((top, left), (bot, right)) =
      ((ys.min, xs.min), (ys.max, xs.max))
  }

  object new_pt {
    private val w = old_pt.right - old_pt.left + 1
    private val h = old_pt.bot   - old_pt.top  + 1

    val ((top, left), (bot, right)) =
      ((old_pt.top - w, old_pt.left - h), (old_pt.bot + w, old_pt.right + h))

    lazy val border: Set[Pos] = Set.from(for {
      x <- left until right
      y = top
    } yield Pos(y, x)).concat(for {
      y <- top until bot
      x = right
    } yield Pos(y, x)).concat(for {
      x <- right until (left, -1)
      y = bot
    } yield Pos(y, x)).concat(for {
      y <- bot until (top, -1)
      x = left
    } yield Pos(y, x))

    val corners: Set[Pos] = Set.from(for {
      y <- List(top, bot)
      x <- List(left, right)
    } yield Pos(y, x))

  }

  pts.indices.foreach(Pt.from)

  /** Represents an element of [[Advent2018.Day6.Day6#pts() pts]], a flag / symbol. */
  case class Pt(id: Int, pos: Pos) {
    val (repr, color) = Pt.repr_map.getOrElseUpdate(id,
      Pt.letter_it.next().toString -> Pt.color_it.next()
    )
  }

  object Pt {
    private val letter_it = ('A' to 'Z').concat('a' to 'z').cycle
    private val color_it = Color.all.slice(3, 16).cycle
    private val repr_map = mutable.Map[Int, (String, Attr)]()

    def from(i: Int): Option[Pt] = Try(Pt(i, pts.apply(i).pos)).toOption
    def from(p: Pos): Option[Pt] = Some(pts.indexWhere(_.pos == p)).filter(_ >= 0).map(i => Pt(i, p))
  }

  /** Represents a position on the grid */
  case class Pos(y: Int, x: Int) {
    def nbrs: Set[Pos] = Set.from(
      for ((y, x) <- List(-1,1,0,0) zip List(0,0,-1,1))
        yield Pos(this.y + y, this.x + x))

    def dist(that: Pos): Int = {
      import math.abs
      abs(this.y - that.y) + abs(this.x - that.x)
    }
  }


  val pts_at    = mutable.Map[Pos, Set[Pt]]() toDefaultMapf (p => Set from (Pt from p))
  val region_of = mutable.Map[Pt, Set[Pos]]() toDefaultMapf (p => Set(p.pos))


  def print_board(): Unit = {
    import old_pt._

    val str = (top to bot).map { y =>
      (left to right).map { x =>
        val pos = Pos(y, x)
        pts_at(pos).toList match {
          case Nil => "."
          case pt :: Nil => (if (pt.pos == pos) Color.Red else pt.color) (pt.repr)
          case _ => Color.Red("#")
        }
      }.mkString("")
    }.mkString("", "\n", "\n")

    println(str)
  }


  print_board()



  def label_plane(): Unit = {
    import new_pt._
    for {
      y <- top to bot
      x <- left to right
    } {
      val here = Pos(y, x)
      val pt_claims = pts minByAll (here dist _.pos)

      pts_at updateWith3 (here, _ ++ pt_claims)

      if (pt_claims.tail.isEmpty)
        region_of updateWith3 (pt_claims.head, _ + here)
    }
  }


  label_plane()
  print_board()



  val valid_pts = pts.toSet diff new_pt.border.flatMap { pos =>
    Some(pts_at(pos).toList) collect { case pt :: Nil => pt }
  }

  valid_pts.map(pt => pt.color(pt.repr) -> region_of(pt).size).maxBy(_._2).print_part1()

  def center_region(): IndexedSeq[Pos] = {
    import new_pt._

    for {
      y <- top to bot
      x <- left to right
      pos = Pos(y, x)
      if pts.map(_.pos dist pos).sum < 10000
    } yield pos
  }

  center_region().size.print_part2()
}
