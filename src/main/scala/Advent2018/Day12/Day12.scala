package Advent2018.Day12

import Advent2018.Base

import scala.collection.mutable

object Day12 extends Base with App {


  val initReader = """initial state: ([#.]+)""".r

  def plantToBit(c: Char): Int = if (c == '#') 1 else 0

  def plantsToByte(s: String): Int = {
    var ret = 0
    for (c <- s) {
      if (c == '#')
        ret += 1
      ret <<= 1
    }
    ret >> 1
  }

  val initState = getLines.next() match {
    case initReader(s) => s.view map plantToBit
  }
  getLines.next()


  val ruleReader = """([#.]{5}) => ([#.])""".r

  val rules = getLines.map {
    case ruleReader(k, v) => plantsToByte(k) -> plantToBit(v.head)
  }.toMap toDefaultMap 0

  var state = IndexedSeq.from(initState)

  var zeroIdx = 0

  def state_get(i: Int): Int = if (i < state.size) state(i) else 0

  def doGen(): IndexedSeq[Int] = {
    var window = state(0)
    zeroIdx -= 2
    for (i <- -2 until (state.length + 2)) yield rules(window) thenDo { _ =>
      window = ((window & 15) << 1) + state_get(i + 3)
    }
  }

  def trimZeroes(iseq: IndexedSeq[Int]) = {
    val drop = iseq.indexOf(1)
    val dropRight = iseq.length - iseq.lastIndexOf(1) - 1
    zeroIdx += drop
    iseq.drop(drop).dropRight(dropRight)
  }

  for (i <- 1 to 20) {
    state = trimZeroes(doGen())
  }

  def dotProdIndices(zeroIdx: Long = zeroIdx) = {
    for ((v, i) <- state.zipWithIndex)
      yield v * (i + zeroIdx)
  }.sum

  dotProdIndices().print_part1()

  zeroIdx = 0
  state = IndexedSeq.from(initState)


  val stateGenIdx = mutable.Map[IndexedSeq[Int], (Int, Long)]()

  var q: Long = 0L
  while (!(stateGenIdx contains state)) {
    stateGenIdx(state) = zeroIdx -> q
    q += 1
    state = trimZeroes(doGen())
  }

  println(
    s"""st  = ${state.mkString("")}
       |old = ${stateGenIdx(state)}
       |new = ${zeroIdx -> q}""".stripMargin
  )

  /** By running the above, we found that identical states
    * occur consecutively.
    * Note that the `zeroIdx` changes tho, so we have to change it appropriately
    * to compute the value at 50 billion. */
  val (prev_zeroIdx, prev_gen) = stateGenIdx(state)
  val moreGens: Long = 50_000_000_000L - prev_gen
  val final_zeroIdx = prev_zeroIdx + (zeroIdx - prev_zeroIdx) * moreGens
  final_zeroIdx.print("last zeroidx: ")
  dotProdIndices(final_zeroIdx).print_part2()

  for (i <- 1 to 999) {
    q += 1
    state = trimZeroes(doGen())
  }

  state.mkString("").print("st  = ")
  (zeroIdx -> q).print("now = ")
}
