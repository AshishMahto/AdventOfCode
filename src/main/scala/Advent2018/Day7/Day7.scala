package Advent2018.Day7

import Shared.Base

import scala.collection.immutable.SortedSet
import scala.collection.{View, mutable}

object Day7 extends Base with App {
  debug_print = false

  val reader = """Step (\w) must be finished before step (\w) can begin.""".r

  val lines = getLines.toList.map { case reader(a, b) => a.head -> b.head }

  val enabledBy = lines.view.groupByf.view.mapValues(_.to(SortedSet)).toMap toDefaultMap SortedSet.empty

  val requiredFor = (for {
    (step, kids) <- enabledBy.view
    kid <- kids
  } yield kid -> step).groupByf.view.mapValues(_.to(SortedSet)).toMap toDefaultMap SortedSet.empty


  val roots = enabledBy.valuesIterator.foldLeft(enabledBy.keySet) { _ diff _ }.to(SortedSet)

  val allSteps = lines.toSet.unzip.combine(_ union _)

  object StepState {
    private val visited: mutable.LinkedHashSet[Char] = mutable.LinkedHashSet[Char]()
    private var ready = roots

    def clear(): Unit = { visited.clear() ; ready = roots }

    def doneStep(s: Char): Unit = {
      visited += s
      ready ++= enabledBy(s) filter (requiredFor apply _ subsetOf visited)
    }

    def popReady(): Char = ready.head thenDo { _ => ready = ready.tail }
    def hasReady: Boolean = ready.nonEmpty
    def popReadyOpt(): Option[Char] = Option.when(hasReady)(popReady())
    def doneAll: Boolean = allSteps subsetOf visited
  }

  new Iterator[Char] {
    override def hasNext: Boolean = StepState.hasReady
    override def next(): Char = StepState.popReady() thenDo StepState.doneStep
  }.mkString("").print_part1()



  case class Elf(step: Char, timeRem: Int) {
    def isDone: Boolean = if (timeRem < 0) throw new IllegalArgumentException else timeRem == 0
    def incr: Elf = if (isDone) throw new IllegalArgumentException else this.copy(timeRem = timeRem - 1)
    def init(step: Char): Elf = Elf.init(step)
    def ready: Elf = Elf.ready
  }

  object Elf {
    def init(step: Char): Elf = Elf(step, (step - 'A') + 60)
    def ready: Elf = new Elf('_', 0)
  }

  @annotation.tailrec
  final def go(
                workers: List[Elf] = (List fill 5)(Elf.ready),
                time: Int = 0,
        ): Int = {
    workers collect { case Elf(c, 0) => StepState.doneStep(c) }
    val ws = workers map { w => if (w.isDone) StepState.popReadyOpt().fold(w.ready)(w.init) else w.incr }

    ws.mkString(f"[$time%02d]:\t", "\t", "").print()

    if ((ws.forall(_.isDone) && StepState.doneAll) || time > 999) time
    else go(ws, time + 1)
  }

  println()
  StepState.clear()
  go().print_part2()
}
