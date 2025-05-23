package Advent2022

import Shared.D

private object D07 extends D {

  sealed trait Tree:
    val size: Long
    val p1: Long
    def p2: Long

  case class File(name: String, size: Long) extends Tree:
    override def toString = s"- $name ($size)\n"
    override val p1 = 0
    override val p2 = Long.MaxValue

  case class Dir(name: String, ls: collection.Seq[Tree] = Nil) extends Tree:
    override val size = ls.view.map(_.size).sum
    override def toString = ls.flatMap(_.toString.linesIterator.map("  " + _)).mkString(s"- $name ($size)\n", "\n", "\n")
    override      val p1 =  (if size < 100000     then size else 0            )  +  ls.view.map(_.p1).sum
    override lazy val p2 = ((if size > needToFree then size else Long.MaxValue) :: ls.toList.map(_.p2)).min

  val file = "(dir|[0-9]+) (.+)".r

  /** Could be purely functional using IO monad, but a bit of encapsulated mutability is scalactic :) */
  val lines = Input.lines.tail.to(collection.mutable.ArrayDeque)
  def create(dirName: String): Dir =
    lines.removeHead() // always ls
    val files = lines removeHeadWhile file.matches collect { case file(sz, name) if sz != "dir" => File(name, sz.toLong) }
    val dirs = Iterator.unfold(-1) { state =>
      lines.removeHeadOption() collect { case s"$$ cd $dirName" if dirName != ".." => (create(dirName), state) }
    }
    Dir(dirName, files ++ dirs)

  val root = create("/")
  val needToFree = root.size - (70000000 - 30000000)

  println(root.toString)
  root.p1.part
  root.p2.part
}
