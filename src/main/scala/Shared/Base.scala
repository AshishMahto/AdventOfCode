package Shared

import java.io.{File, FileNotFoundException}

import scala.io.Source
import scala.language.implicitConversions


trait Base extends Library {

  val debug_path = false

  private val path_to_days = List("src", "main", "scala").mkString(File.separator)

  val dir = new File(path_to_days)

  if (debug_path) {
    println(
      s"""Your current directory is: '${dir.getCanonicalPath}'
         |If the above directory is not the folder immediately upwards from the Advent20XX folders,
         |Please edit the `Shared.Base.path_to_days` to make this the case. Use ".." to move up a folder if needed.
         |""".stripMargin)
  }

  assert(dir.isDirectory)

  private val concrete_class_name = getClass.getCanonicalName.split('.').init.mkString(File.separator)
  private val path_to_input = List(path_to_days, concrete_class_name, "input.txt").mkString(File.separator)

  val input_file = new File(path_to_input)

  if (! input_file.exists()) throw new FileNotFoundException(
      s"Could not find file $path_to_input in directory ${dir.getPath}."
  )

  private val inp = Source.fromFile(input_file)

  /** Returns all of the lines of input from a file named `input.txt`, as an iterator. <br>
    * Automatically closes the input file once the iterator is empty. <br>
    * @see [[scala.io.Source#getLines()]]
    */
  val getLines: Iterator[String] = new Iterator[String] {
    val inner: Iterator[String] = inp.getLines()

    // might close the file twice... is that bad?
    override def hasNext: Boolean = inner.hasNext thenDo { x => if (!x) inp.close() }

    override def next(): String = inner.next()
  }

  lazy val firstLine: String = getLines.next() thenDo { _ => inp.close() }
}
