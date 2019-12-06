package Advent2019.Day6

import Shared.Base

import scala.collection.View

object Day6 extends Base with App {
  val lns = List(
    getLines.toList,
    """COM)B
      |B)C
      |C)D
      |D)E
      |E)F
      |B)G
      |G)H
      |D)I
      |E)J
      |J)K
      |K)L
      |K)YOU
      |I)SAN""".stripMargin.linesIterator.toList
  )(0)

  val reader = """(\w+)\)(\w+)""".r

  val graph = lns.map {
    case reader(a,b) => a -> b
  }.groupByf.toDefaultMap(Nil)

  def rec_size(key: String): Int = {
    graph(key).size + graph(key).map(rec_size).sum
  }

  graph.keysIterator.map(rec_size).sum.print_part1()

  def find_val(key: String, dest: String): Option[Int] = {
    if (key == dest) Some(0)
    else graph(key).flatMap(k => find_val(k, dest)).headOption.map(_ + 1)
  }

  def find_path(key: String): Option[Int] = {
    graph(key).flatMap(find_path).headOption orElse (for {
      san <- find_val(key, "SAN")
      you <- find_val(key, "YOU")
    } yield san + you - 2 thenDo { _ => println(s"key = $key, san = $san, you = $you") })
  }

  val inv_graph = (for {
    (k, v) <- graph.toList
    s <- v
  } yield s -> k).groupByf.toDefaultMap(Nil)

  graph.keysIterator.flatMap(find_path).nextOption().print_part2()
}
