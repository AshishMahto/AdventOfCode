package Advent2024

import Shared.D

private[this] object D04 extends D {
//  override protected val input =
  """MMMSXXMASM
    |MSAMXMSMSA
    |AMXSXMAAMM
    |MSAMASMSMX
    |XMASAMXAMM
    |XXAMMXXAMA
    |SMSMSASXSS
    |SAXAMASAAA
    |MAMMMXMMMM
    |MXMXAXMASX""".stripMargin
  val mat = Input.lines.toVector
  val leftright = mat
  val updown = mat.transpose.map(_.mkString(""))
  val totalDiags = mat.length + mat(0).length - 1
  val diagLeft = (3 until (totalDiags - 3)).map { i =>
    mat.zipWithIndex.map { case (row, j) =>
      if 0 <= i - j && i - j < row.length then row(i - j).toString else ""
    }.mkString("")
  }
  val diagRight = (4 until (totalDiags - 2)).map { i0 =>
    val i = i0 - mat.length
    mat.zipWithIndex.map { case (row, j) =>
      if 0 <= i + j && i + j < row.length then row(i + j).toString else ""
    }.mkString("")
  }
  val all = List(leftright, updown, diagLeft, diagRight)
  all.flatten.map { row => "(?=XMAS|SAMX)".r.findAllMatchIn(row).size }.sum.part

  val X = List(
    (0, 0),         (2, 0),
            (1, 1),
    (0, 2),         (2, 2)
  )
  val valid = Set("MMASS", "SMASM", "MSAMS", "SSAMM")
  (for {
    y <- 2 until mat.length
    x <- 2 until mat(0).length
    if valid(X.map { case (dx, dy) => mat(y - dy)(x - dx) }.mkString(""))
  } yield 1).sum.part
}
