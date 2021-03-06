package demo

import java.io.File
import java.text.SimpleDateFormat
import java.util.{Date, Calendar, GregorianCalendar, Locale}
import scala.annotation.implicitNotFound
import scala.util.control.{Exception => Except}


@implicitNotFound("No member of type class Reader in scope for ${T}")
trait Reader[T] {
  def read(s: String): T

  def readOption(s: String): Option[T] =
    Except.allCatch.opt(read(s))
}

/*
  Grammar:
    tuples: (x | y)
    lists: x, y, z
    lists of tuples: (x | y), (r | s)
 */


object Reader {

  val commaDelim = """\s*,\s*""".r
  val tupleDelim = """\s*\|\s*""".r
  val tupleStart = """\s*\(\s*([^|]+)""".r
  val tupleEnd = """([^|]+)\s*\)\s*""".r


  // The simple ones first...
  //def read[T](s: String)(implicit ev: Reader[T]): T = ev.read(s)
  // OR...
  def read[T: Reader](s: String): T = implicitly[Reader[T]].read(s)


  // Now we can get fancy using 'apply...'
  // For direct construction of a Readable instance
  //def apply[T: Reader]: Reader[T] = implicitly[Reader[T]]

  // Same as:
  def apply[T](implicit instance: Reader[T]): Reader[T] = instance

  // With this we can just supply the String as an argument to the Reader object itself
  def apply[T: Reader](s: String): T = implicitly[Reader[T]].read(s)


  // A reader of one type can be transformed to another
  private def xform[A,B](reader: Reader[A])(f: A => B): Reader[B] = new Reader[B] {
    override def read(s: String): B = f(reader.read(s))
  }

  // To make construction cleaner
  private def toReader[T](f: String => T): Reader[T] = new Reader[T] {
    def read(s: String) = f(s)
  }

  implicit val intRead: Reader[Int] = toReader(_.toInt)

  implicit val doubleRead: Reader[Double] = toReader(_.toDouble)

  implicit val integerRead: Reader[Integer] = toReader(s => Integer.parseInt(s))

  implicit val stringRead: Reader[String] = toReader(identity)

  implicit val booleanRead: Reader[Boolean] = toReader(_.toBoolean)

  implicit val calendarRead: Reader[Calendar] = calendarReader("yyyy-MM-dd")

  implicit val dateRead: Reader[Date] = toReader { s =>
    val df = new SimpleDateFormat("yyyy-MM-dd")
    df.parse(s)
  }

  implicit val fileRead: Reader[File] = toReader(new File(_))

  private def calendarReader(pattern: String): Reader[Calendar] = calendarReader(pattern, Locale.getDefault)

  private def calendarReader(pattern: String, locale: Locale): Reader[Calendar] =
    toReader { s =>
      val fmt = new SimpleDateFormat(pattern, locale)
      val c = new GregorianCalendar
      c.setTime(fmt.parse(s))
      c
    }


  // Even implicits can take implicits as parameters!
  implicit def tupleRead[A: Reader, B: Reader]: Reader[(A, B)] = toReader(s => {
    val a = tupleDelim.split(s)
    Reader[A].read(tupleStart.replaceAllIn(a.head, _.group(1))) ->
      Reader[B].read(tupleEnd.replaceAllIn(a.last, _.group(1)))
  })

  // Notice there is no need to use 'implicitly' to bring Reader[S] into scope.
  implicit def seqRead[S: Reader]: Reader[Seq[S]] =
    toReader[Seq[S]](s => commaDelim.split(s).map(Reader[S].read).toSeq)

  // implicit argument
  //implicit def listRead[S](implicit ev: Reader[Seq[S]]): Reader[List[S]] = xform(ev)(_.toList)

  // Or we can use context bounds syntax
  implicit def listRead[S: Reader]: Reader[List[S]] = xform(Reader[Seq[S]])(_.toList)


  object ops {
    implicit class stringWithReader(s: String) {
      def read[T: Reader] = implicitly[Reader[T]].read(s)
      def readOption[T: Reader] = implicitly[Reader[T]].readOption(s)
    }
  }

}


object Main {

  import Reader.ops._

  implicit val fileRead: Reader[File] = new Reader[File] {
    override def read(s: String): File = new File(s)
    override def readOption(s: String): Option[File] = {
      val file = read(s)
      if(file.exists()) Some(file) else None
    }
  }


  val ints = "1, 2, 3, 4, 5"
  val tuples = "(one | 1), (two|  2), (three |3)"
  val date = "2016-03-16"

  def main(args: Array[String]): Unit = {

    println("9".read[Double])

    println(Reader.read[List[Int]](ints))

    println(Reader[List[Double]](ints))

    println(Reader[List[Int]].read(ints).mkString(", "))
    println(Reader[List[(String,Int)]].read(tuples).mkString(", "))
    println(Reader[Calendar].read(date).getTime)

    println(Reader[Calendar](date).getTime)

    println(Reader[Date](date))

    println(date.read[Date])

    "1944-06-06".read[Date]

    println("true".readOption[Boolean])

    println("myFile".readOption[File])

  }
}
