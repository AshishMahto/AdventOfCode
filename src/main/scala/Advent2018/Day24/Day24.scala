package Advent2018.Day24

import Shared.Base

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.collection.mutable


//noinspection TypeAnnotation
object Day24 extends Base with App {

  case class Defense(hp: Int, immune: Set[Symbol], weak: Set[Symbol])

  object Defense {
    val reader =
      """(\d+) hit points(?: \((?:(immune|weak) to ([^);]+))?(?:; )?(?:(?:immune|weak) to ([^);]+))?\))?""".r

    def words_to_set(s: String): Set[Symbol] =
      if (s == null) Set.empty else s.split(", ").toSet.map(Symbol.apply)

    def from(s: String) = {
      val reader(hp, tpe1, val1, val2) = s
      val (imm, weak) = if (Symbol(tpe1) == Symbol("immune")) val1 -> val2 else val2 -> val1
      Defense(hp.toInt, words_to_set(imm), words_to_set(weak))
    }
  }

  case class Offense(dmg: Int, tpe: Symbol, initiative: Int)

  object Offense {
    val reader = """(\d+) (\w+) damage at initiative (\d+)""".r
    def from(s: String): Offense = {
      val reader(dmg, tpe, init) = s
      Offense(dmg.toInt, Symbol(tpe), init.toInt)
    }
  }

  case class Group(size: Int, defense: Defense, offense: Offense) {
    @inline def effectivePower = size * offense.dmg
    @inline def initiative = offense.initiative

    def simulatedDmg(u: Group): Int = u match {
      case Group(_, Defense(_, immunities, weaknesses), _) =>
        import u.offense.tpe
             if (immunities contains tpe) 0
        else if (weaknesses contains tpe) 2 * effectivePower
        else effectivePower
    }

    def attack(u: Group): Group = {
      val num_lost = simulatedDmg(u) / u.defense.hp
      u.copy(size = u.size - num_lost)
    }

    def selectTarget(choices: collection.Set[Group]): Option[Group] =
      choices.maxByOption(u => simulatedDmg(u) -> u.effectivePower)
  }

  object Group {
    val reader = """(\d+) units each with (.+?) with an attack that does (.+)""".r
    def from(s: String): Group = {
      val reader(sz, df, atk) = s
      Group(sz.toInt, Defense.from(df), Offense.from(atk))
    }
  }


  val (immuneSys, infection) = {
    val (_:: tk,
    _ :: _ :: dr) = getLines.toList.span(_.nonEmpty)

    def eval(lns: List[String]) = SortedMap from lns.map { s =>
      val u = Group from s
      u.initiative -> u
    }

    eval(tk) -> eval(dr)
  }


  def findTargets(atk: collection.Seq[Group],
                  df : collection.SortedMap[Int, Group]): Map[Int, Int] = {
    val not_visited = df.valuesIterator.to(mutable.Set)
    Map.from(for {
      u <- atk.view
      t <- u.selectTarget(not_visited)
      true = not_visited remove t
    } yield u.initiative -> t.initiative)
  }



}
