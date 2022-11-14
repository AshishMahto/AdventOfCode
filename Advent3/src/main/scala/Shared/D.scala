package Shared

import java.io.File
import java.io.File.separator as \
import java.lang.System.nanoTime
import java.net.HttpCookie
import scala.io.Source
import scala.util.matching.Regex
import java.nio.file.{Files, Path, StandardOpenOption}
import requests.RequestBlob.FormEncodedRequestBlob

//noinspection ScalaUnusedSymbol
trait Helpers { self =>
  protected var debug_print: Boolean = true
  private var level = 0
  extension[T] (x: T)
    def thenDo(f: T => Unit): T =
      f(x)
      x
    def pr(pfx: String = ""): Unit = if (debug_print) println(pfx + " " + x)
    def part: Unit =
      level += 1
      println(s"Part $level Answer: " + x)
      self match
        case d: D => d.answer(level, x.toString).thenDo(_.foreach(_ pr "response = "))
        case _ => ()
  def pLines(ls: Any*): Unit = if (debug_print) ls foreach println
  def time[R](block: => R): R =
    val t0 = nanoTime()
    try block finally ((nanoTime() - t0) / 1e9).pr("Time: ")
}

//noinspection ScalaUnusedSymbol
trait D extends Helpers:
  private val cookie = "session=***REMOVED***"
  private val sesh = requests.Session(headers = Map("cookie" -> cookie))
  private val (year, day, day0) =
    val Seq(year, day) = raw"\d+".r findAllIn this.getClass.getName to Seq
    (year, day.stripPrefix("0"), f"${day.toInt}%02d")
  private val inFile = new File(s"dir$year${\}day$day0.inp.txt")
  private def adventURL(s: String = "") = s"https://adventofcode.com/$year/day/$day/$s".stripSuffix("/")

  val input = if inFile.exists() then Files readString inFile.toPath else sesh.get(adventURL("input")).text()

  object Input:
    def nums = raw"\d+".r findAllIn input map (_.toInt)

  private[Shared] def answer(level: Int, answer: String) = 
    val lookFor = "(?<=<p>)[^<]+".r
    val outFile = new File(s"dir$year${\}day$day0-$level.outs.txt")
    val txt = if !outFile.exists() then
      outFile.createNewFile()
      ""
    else Files readString outFile.toPath
    if (txt contains answer) None else
      Files.writeString(outFile.toPath, answer + "\n", StandardOpenOption.APPEND)
      lookFor findFirstIn
        sesh.post(adventURL("answer"), data = Map("level" -> level.toString, "answer" -> answer)).text() map
        Answer.from

  if !inFile.getParentFile.isDirectory then inFile.getParentFile.mkdir()
  if !inFile.exists() then Files.writeString(inFile.toPath, input, StandardOpenOption.CREATE_NEW)

  def main(args: Array[String]): Unit = ()

sealed trait Answer
object Answer {
  case class Wait(seconds: Int) extends Answer
  object Wait:
    val rgx = raw"You gave an answer too recently; you have to wait after submitting an answer before trying again\.  You have (\d+)m (\d+)s left to wait\.".r
  case class Undefined(p: String) extends Answer
  sealed trait SimpleAnswer(val text: String) extends Answer
  case object NotRight extends SimpleAnswer("That's not the right answer.  If you're stuck, make sure you're using the full input data; there are also some general tips on the ")
  case object Correct extends SimpleAnswer("That's the right answer!  You are ")
  case object WrongLevel extends SimpleAnswer("You don't seem to be solving the right level.  Did you already complete it? ")
  object SimpleAnswer:
    val all = List[SimpleAnswer](NotRight, Correct, WrongLevel)

  def from(s: String): Answer = SimpleAnswer.all.find(s == _.text).getOrElse(s match
    case Wait.rgx(minutes, seconds) => Wait(60 * minutes.toInt + seconds.toInt)
    case _ => Undefined(s)
  )
}
