package Advent2022

import Shared.D

private object D04 extends D {
//  override val input = 
  """2-4,6-8
    |2-3,4-5
    |4-5,2-3
    |5-7,7-9
    |2-8,3-7
    |6-6,4-6
    |2-6,4-8""".stripMargin
  val parsed = Input.lines.map { _.split(",") flatMap (_ split '-') map (_.toInt) match
    case Array(a, b, c, d) => (a, b, c, d)
  }
  def order(a: Int, b: Int, c: Int, d: Int) = a <= b && c <= d
  
  parsed.count { case (a, b, x, y) => order(a, x, y, b) || order(x, a, b, y) }.part
  
  parsed.count { case (a,b,x,y) => order(a,x,x,b) || order(a,y,y,b) || order(x,a,a,y) || order(x,b,b,y) }.part
}
