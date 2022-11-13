package Advent2021

import Shared.D

import java.io.File

object D1 extends D {
  val ls = Input.nums.toList
  ls.zip(ls.tail).count { case (a, b) => b > a }.part
  ls.sliding(4).count { ls =>
    val List(a, _, _, b) = ls
    b > a
  }.part
}
