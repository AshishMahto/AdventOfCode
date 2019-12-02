package Advent2018.Day13

import Shared.Base

object Day13 extends Base with App {

  case class Pt(y: Int, x: Int) {
    def +(that: Pt) = Pt(y + that.y, x + that.x)

    def *:(k: Int) = Pt(k * y, k * x)
  }

  trait Location {
    val loc: Pt
  }

  class WithLocation(locable: Location) extends Location {
    override val loc: Pt = locable.loc
  }


  sealed trait Cardinal {
    val dir: Pt
    def opposite: Cardinal = ???
    
    
    def +(pt: Pt): Pt = dir + pt
  }

  object Cardinal {

    case object N extends Cardinal { 
      override val dir = Pt( 1, 0) 
    }
    
    case object S extends Cardinal {
      override val dir = Pt(-1, 0) 
    }
    
    case object E extends Cardinal {
      override val dir = Pt( 0, 1)
    }
    
    case object W extends Cardinal {
      override val dir = Pt( 0,-1) 
    }

    implicit def toPt(c: Cardinal): Pt = c.dir
  }

  import Cardinal.{N, S, E, W}

  val reprs = Map[String, List[Set[Cardinal]]](
    "|" -> List(Set(N, S)),
    "\\" -> List(Set(N, E), Set(S, W)),
    "/" -> List(Set(N, W), Set(S, E)),
    "-" -> List(Set(E, W)),
    "+" -> List(Set(N, S, E, W))
  )


  sealed class Orientation(val exits: Set[Cardinal]) {
    assert(exits.sizeCompare(1) == 0, "Must be exactly 2 distinct exits.")

    def this(exit1: Cardinal, exit2: Cardinal) = this(Set(exit1, exit2))

    def otherExit(exit: Cardinal): Cardinal = (exits - exit).head
  }


  object Orientation {
    implicit def toTuple(o: Orientation): (Cardinal, Cardinal) = o.exits.head -> o.exits.last

    case object Vertical    extends Orientation(N, S)
    case object Horinzontal extends Orientation(E, W)
    case object NorthEast   extends Orientation(N, E)
    case object NorthWest   extends Orientation(N, W)
    case object SouthEast   extends Orientation(S, E)
    case object SouthWest   extends Orientation(S, W)
  }


  sealed trait Tracks extends Location {
    val repr: String
  }

  object Tracks {

    case class Intersection(loc: Pt) extends Tracks {
      override val repr = "+"
    }

    case class Standard(dir: Orientation, loc: Pt) extends Tracks {
      override val repr: String = reprs.find(_._2 contains dir.exits).get._1
    }

  }

  val trackAt: Map[Pt, Tracks] = null

  case class Cart(facing: Cardinal, on: Tracks) extends WithLocation(on) {
    val nextTrack: Tracks = trackAt.getOrElse(facing + this.loc,
      throw new IllegalArgumentException("Cart should be `facing` a direction with a track on it.")
    )

    /** Moves the cart 1 space on the track, computing a new valid 'facing'. */
    def incr: Cart = {
      null
    }
  }




}
