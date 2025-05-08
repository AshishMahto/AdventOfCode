package Advent2024

import Shared.D

import collection.mutable
import fansi.Color

private object D09 extends D {
//  override val input = "1771316180312550163633387627715459675444493468456487931654296466706737971886348217818".slice(0, 19)
//  override val input = "2333133121414131402"

  def isFile(index: Int) = index % 2 == 0

  def part1() =
    val arr = Input.str.view.map(_ - '0').toArray
    var ans = 0L
    var leftBlocks = 0
    var index = 0
    var lastIndex = arr.length - 1
    while index <= lastIndex do
      if isFile(index) then ans += index/2L * (triangle(arr(index) + leftBlocks) - triangle(leftBlocks))
      else
        var borrowed = 0
        while borrowed < arr(index) do
          val newBorrowed = arr(lastIndex).min(arr(index) - borrowed)
          ans += lastIndex/2L * (triangle(newBorrowed + borrowed + leftBlocks) - triangle(borrowed + leftBlocks))
          if newBorrowed == arr(lastIndex) then lastIndex -= 2 else arr(lastIndex) -= newBorrowed
          borrowed += newBorrowed
      leftBlocks += arr(index)
      index += 1
    ans

  part1().part

  val arr = Input.str.view.map(_ - '0').toArray

  // sorted index -> size
  val emptyChunks = mutable.SortedMap[Int, Int]()
  val fileIndexes = mutable.ArrayBuffer.fill(arr.length / 2 + 1)(0)

  arr.view.zipWithIndex.foldLeft(0) { case (acc, (size, idx)) =>
    if !isFile(idx) then emptyChunks(acc) = size else fileIndexes(idx / 2) = acc
    acc + size
  }

  val expanded = arr.view.zipWithIndex.flatMap { case (size, i) =>
    if isFile(i) then List.fill(size)(i/2) else List.fill(size)(0)
  }.to(mutable.ArrayBuffer)

  arr.zipWithIndex.reverse.foreach { case (size, id) =>
    if isFile(id) then
      val src = fileIndexes(id/2)
      emptyChunks --= emptyChunks.iteratorFrom(src+1).map(_._1)
      emptyChunks collectFirst { case elem @ (dest, emptySize) if size <= emptySize =>
        expanded.patchInPlace(dest, Iterator.fill(size)(id / 2), size)
        expanded.patchInPlace(src,  Iterator.fill(size)(0), size)
        emptyChunks.remove(dest)
        if emptySize > size then emptyChunks(dest + size) = emptySize - size
      }
  }
  expanded.zipWithIndex.map { case (id, i) => id.toLong * i }.sum.part
}
