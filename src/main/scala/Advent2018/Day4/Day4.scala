package Advent2018.Day4

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.{ChronoUnit, TemporalUnit}

import Advent2018.Base

object Day4 extends Base with App {

  sealed trait Info
  case object Asleep extends Info
  case object Wake extends Info
  case class BeginShift(id: Int) extends Info

  object Info {
    private val beginShift = """Guard #(\d+) begins shift""".r
    private val asleep = "falls asleep"
    private val wake = "wakes up"

    def from(str: String): Info = str match {
      case `asleep` => Asleep
      case `wake` => Wake
      case beginShift(id) => BeginShift(id.toInt)
      case x => throw new AssertionError(s"Could not parse '$x'")
    }
  }

  case class Event[MyInfo <: Info](when: LocalDateTime, what: MyInfo)

  object Event {
    private val reader = """(\[.+\]) (.+)""".r
    private val fmt = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

    def from(ln: String): Event[Info] = {
      val (date, info) = ln.splitAt(ln.indexOf(']'))
      Event(LocalDateTime.parse(date.tail, fmt), Info.from(info.drop(2)))
    }

    implicit val eventOrder: Ordering[Event[Info]] = Ordering.by(_.when)
  }

  val inp =
    """[1518-11-01 00:00] Guard #10 begins shift
      |[1518-11-01 00:05] falls asleep
      |[1518-11-01 00:25] wakes up
      |[1518-11-01 00:30] falls asleep
      |[1518-11-01 00:55] wakes up
      |[1518-11-01 23:58] Guard #99 begins shift
      |[1518-11-02 00:40] falls asleep
      |[1518-11-02 00:50] wakes up
      |[1518-11-03 00:05] Guard #10 begins shift
      |[1518-11-03 00:24] falls asleep
      |[1518-11-03 00:29] wakes up
      |[1518-11-04 00:02] Guard #99 begins shift
      |[1518-11-04 00:36] falls asleep
      |[1518-11-04 00:46] wakes up
      |[1518-11-05 00:03] Guard #99 begins shift
      |[1518-11-05 00:45] falls asleep
      |[1518-11-05 00:55] wakes up""".stripMargin

  val events = getLines.map(Event.from).toList.sorted.explode(_.what.isInstanceOf[BeginShift]).collect {
    case Event(t, bs: BeginShift) :: rest => Event(t, bs) -> rest.grouped(2).toList
  }.toList

  val asleep_seconds = List.from(for {
    (Event(_, BeginShift(id)), rests) <- events
    List(Event(t1, Asleep), Event(t2, Wake)) <- rests
    i <- t1.getMinute until t2.getMinute
  } yield id -> i).groupBy(_._1).view.mapValues { ls => ls.map(_._2).freq_map }



  val (sleepy_guard, sleepy_sched) = asleep_seconds.maxBy(_._2.values.sum)

  (sleepy_guard * sleepy_sched.maxBy(_._2)._1).print_part1()



  val (consistent_guard, sched) = asleep_seconds.maxBy(_._2.values.max)

  (consistent_guard * sched.maxBy(_._2)._1).print_part2()
}
