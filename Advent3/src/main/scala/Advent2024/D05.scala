package Advent2024

import Shared.D

import scala.collection.mutable

private object D05 extends D {
//  override protected val input =
  """47|53
    |97|13
    |97|61
    |97|47
    |75|29
    |61|13
    |75|53
    |29|13
    |97|29
    |53|29
    |61|53
    |97|53
    |61|29
    |47|13
    |75|47
    |97|75
    |47|61
    |75|61
    |47|29
    |75|13
    |53|13
    |
    |75,47,61,53,29
    |97,61,53,29,13
    |75,29,13
    |75,97,47,61,53
    |61,13,29
    |97,13,75,29,47""".stripMargin



  val Array(dirs0, updates0) = Input.str.split("\n\n")
  val fwdStep = new Map.WithDefault(
    dirs0.split("\n").map { _.split('|').map(_.toInt).toList}.groupMapReduce(_.head)(xy => Set.from(xy.tail))((x, y) => y | x),
    _ => Set.empty[Int]
  )
  type ISeq = collection.IndexedSeq[Int]
  val updates = updates0.split('\n').toList.map { _.split(',').map(_.toInt).toVector }
  def checkUpdate(update: ISeq) = update zip update.view.tail forall { case (a, b) =>
    fwdStep(a)(b)
  }

  def middle(update: ISeq) = update(update.length / 2)
  updates.filter(checkUpdate).map(middle).sum.part

  def fix(update: ISeq): ISeq = {
    val sorted = update.to(mutable.ArraySeq)
    while (!checkUpdate(sorted)) {
      for (i <- 1 until sorted.length) {
        val a = sorted(i - 1)
        val b = sorted(i)
        if !fwdStep(a)(b) then {
          sorted(i - 1) = b
          sorted(i) = a
        }
      }
    }
    sorted
  }
  updates.filterNot(checkUpdate).map(fix).map(x => middle(x)).sum.part
}
