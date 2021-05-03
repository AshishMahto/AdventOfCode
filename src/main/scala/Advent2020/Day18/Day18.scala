package Advent2020.Day18

import Shared.Base

object Day18 extends Base with App {
  type Int = BigInt
  trait Expr {
    def eval: Int
  }
  case class Mul(l: Expr, r: Expr) extends Expr {
    override def toString = s"($l * $r)"
    def eval = l.eval * r.eval
  }
  case class Add(l: Expr, r: Expr) extends Expr {
    override def toString = s"($l + $r)"
    def eval = l.eval + r.eval
  }
  case class Num(eval: Int) extends Expr {
    override def toString = eval.toString
  }

  object Parser {
    import fastparse._, ScalaWhitespace._
    def number[_: P]: P[Expr] = P( CharIn("0-9").rep(1).!.map(x => Num(x.toInt)))
    def parens[_: P]: P[Expr] = P( "(" ~/ mul ~ ")" )
    def factor[_: P]: P[Expr] = P( number | parens )

    def add[_: P]: P[Expr] = P( factor ~ ("+" ~/ factor).rep ).map {
      case (l, rs) => rs.foldLeft(l){ case (l, r) => Add(l, r) }
    }

    def mul[_: P]: P[Expr] = P( add ~ ("*" ~/ add).rep ).map {
      case (l, rs) => rs.foldLeft(l){ case (l, r) => Mul(l, r) }
    }

    def expr[_: P]: P[Expr] = P( mul ~ End )

    def apply(s: String) = parse(s, expr(_)).get.value
  }

  println(getLines.map(s => Parser(s).eval).sum)
}
