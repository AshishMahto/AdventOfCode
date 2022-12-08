package Shared

import java.io.File
import java.io.File.separator as \
import java.lang.System.nanoTime
import java.net.HttpCookie
import scala.io.Source
import scala.util.matching.Regex
import java.nio.file.{Files, Path, StandardOpenOption}
import requests.RequestBlob.FormEncodedRequestBlob

import scala.annotation.targetName
import scala.util.Try

trait Helpers { self =>
  protected var debug_print: Boolean = true
  private var level = 0
  type ValidAnswer = Int | Long | String
  extension[T] (x: T)
    def thenDo(f: T => Unit): T =
      f(x)
      x
    def pr(pfx: String = ""): Unit = if (debug_print) println(pfx + " " + x)
    def part: Unit =
      level += 1
      println(s"Part $level Answer: " + x)
      (self, x) match
        case (d: D, x: ValidAnswer) => d.answer(level, x.toString).thenDo(_.foreach(_ pr "response ="))
        case (d: D, _)              => println(s"answer has type ${x.getClass.getSimpleName}, which is not a ValidAnswer")
        case _                      => ()
  extension (x: Int)
    @targetName("modulus")
    def %%(y: Int) = java.lang.Math.floorMod(x, y)
    def thousand: Int = x * 1000
    def million: Int = x * 1000_000
    def billion: Int = x * 1000_000_000
  extension (x: Long)
    @targetName("modulusL")
    def %%(y: Long) = java.lang.Math.floorMod(x, y)
    def thousand: Long = x * 1000
    def million: Long = x * 1000_000
    def billion: Long = x * 1000_000_000
  def pLines(ls: Any*): Unit = if (debug_print) ls foreach println
  def time[R](block: => R): R =
    val t0 = nanoTime()
    try block finally ((nanoTime() - t0) / 1e9).pr("Time: ")
  // truthy
  given Conversion[Try[?], Boolean] = _.isSuccess
  given Conversion[Iterable[?], Boolean] = _.nonEmpty
  given Conversion[IterableOnce[?], Boolean] = _.knownSize match
    case -1 => throw IllegalArgumentException("Expected IterableOnce with knownSize")
    case 0  => false
    case _  => true
}

trait D extends Helpers:
  private val sesh = requests.Session(headers = Map("cookie" -> Secrets.cookie))
  private val (year, day, day0) =
    val Seq(year, day) = raw"\d+".r findAllIn this.getClass.getName to Seq
    (year, day.stripPrefix("0"), f"${day.toInt}%02d")
  private val inFile = new File(s"dir$year${\}day$day0.inp.txt")
  private def adventURL(s: String = "") = s"https://adventofcode.com/$year/day/$day/$s".stripSuffix("/")

  protected val input: String = null
  private lazy val input0: String = 
    if input != null        then input 
    else if inFile.exists() then Files readString inFile.toPath 
    else                         sesh.get(adventURL("input")).text()

  object Input:
    lazy val str: String = input0.trim
    lazy val nums = raw"\d+".r findAllIn input0 map (_.toInt)
    lazy val lines = input0.linesIterator.toList

  import scala.jdk.CollectionConverters._
  private[Shared] def answer(level: Int, answer: String) = if input != null then Some(Answer.SampleInput) else
    val lookFor = "(?<=<p>)[^<]+".r
    val outFile = new File(s"dir$year${\}day$day0-$level.outs.txt")
    val txt = if !outFile.exists() then
      outFile.createNewFile()
      collection.mutable.Buffer.empty[String]
    else Files.readAllLines(outFile.toPath).asScala
    if (txt contains answer) None else
      lookFor findFirstIn
        sesh.post(adventURL("answer"), data = Map("level" -> level.toString, "answer" -> answer)).text() map
        Answer.from thenDo { ans =>
        Files.writeString(outFile.toPath, answer + "\n" + ans.fold("")(_.toString + "\n"), StandardOpenOption.APPEND)
      }

  if !inFile.getParentFile.isDirectory then inFile.getParentFile.mkdir()
  if !inFile.exists() && null == input then Files.writeString(inFile.toPath, input0, StandardOpenOption.CREATE_NEW)

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
  case class IsToo(dir: String) extends Answer
  object IsToo:
    val rgx = raw"That's not the right answer; your answer is too ([a-z]+)\.  If you're stuck, make sure you're using the full input data; there are also some general tips on the ".r
  object SomeoneElseAnswer:
    val rgx = raw"That's not the right answer; your answer is too ([a-z]+)\.  Curiously, it's the right answer for someone else; you might be logged in to the wrong account or just unlucky. In any case, you need to be using ".r
  object SimpleAnswer:
    val all = List[SimpleAnswer](NotRight, Correct, WrongLevel)
  case object SampleInput extends Answer

  def from(s: String): Answer = SimpleAnswer.all.find(s == _.text).getOrElse(s match
    case Wait.rgx(minutes, seconds) => Wait(60 * minutes.toInt + seconds.toInt)
    case IsToo.rgx(dir) => IsToo(dir)
    case SomeoneElseAnswer.rgx(dir) => IsToo(dir)
    case _ => Undefined(s)
  )
}
