package Advent2018.Day9

import Advent2018.Base

import scala.collection.mutable

object Day9 extends Base with App {

  val reader = """(\d+) player(?:s?); last marble is worth (\d+) point(?:s?)""".r

  var (num_players, num_marbles) = {
    getLines.next() match {
      case reader(plyrs, mrbls) => plyrs.toInt -> mrbls.toInt
    }
    // 10 -> 1618
  }

  def go(num_players: Int, num_marbles: Int) = {
    val marbles = new Mutable.Cycle[Long]()
    marbles.addOne(0)

    object get_marble {
      var cur_marble = 1
      def apply(): Int = cur_marble thenDo { _ => cur_marble = cur_marble + 1 }
    }

    object get_plyr {
      var cur_plyr = 1
      def apply(): Int = cur_plyr thenDo { _ => cur_plyr = (cur_plyr % num_players) + 1 }
    }

    val scores = mutable.Map[Int, Long]().toDefaultMap(0)

    def do_round(): Unit = {
      val cur_marble = get_marble()
      val cur_plyr = get_plyr()

      if (cur_marble % 23 == 0) {
        scores.updateWith3(cur_plyr, _ + cur_marble)
        marbles.rotate(-7)
        scores.updateWith3(cur_plyr, _ + marbles.pop())
      } else {
        marbles.rotate(2)
        marbles.addOne(cur_marble)
      }
    }

    for (_ <- 1 to num_marbles) {
      val p = get_plyr.cur_plyr
      do_round()
      // marbles.print(s"[$p] ")
    }
    println("")
    scores
  }

  go(num_players, num_marbles).maxBy(_._2).print_part1()

  val big_score = go(num_players, 100 * num_marbles)
  big_score.maxBy(_._2).print_part2()
  if (0 > big_score.values.min) println("Warning: Answer 2 is incorrect, `long` isn't large enough")
}
