package demo

import java.io.File
import java.text.SimpleDateFormat
import java.util.{Calendar, GregorianCalendar, Locale}

import scala.annotation.implicitNotFound



@implicitNotFound("No member of type class Reader in scope for ${T}")
trait Reader[T] {
  def read(s: String): T

  // TODO Add readOption ?
}

/*
  Grammar:
    tuples: (x | y)
    lists: x, y, z
 */


object Reader {

  val commaDelim = """\s*,\s*""".r
  val tupleDelim = """\s*\|\s*""".r
  val tupleStart = """\s*\(\s*([^|]+)""".r
  val tupleEnd = """([^|]+)\s*\)\s*""".r


  private def toReader[T](f: String => T): Reader[T] = new Reader[T] {
      def read(s: String) = f(s)
  }

  // For direct construction of a Readable instance
  def apply[T: Reader]: Reader[T] = implicitly[Reader[T]]
  // Same as:
  // def apply[T](implicit instance: Reader[T]): Reader[T] = instance


  // Not sure we need this (?)
  private def map[A,B](reader: Reader[A])(f: A => B): Reader[B] = new Reader[B] {
    override def read(s: String): B = f(reader.read(s))
  }

  implicit val intRead: Reader[Int] = toReader(_.toInt)

  implicit val doubleRead: Reader[Double] = toReader(_.toDouble)

  implicit val integerRead: Reader[Integer] = toReader(s => Integer.parseInt(s))

  implicit val stringRead: Reader[String] = toReader(identity)

  implicit val booleanRead: Reader[Boolean] = toReader(_.toBoolean)

  implicit val calendarRead: Reader[Calendar] = calendarReader("yyyy-MM-dd")

  implicit val fileRead: Reader[File] = toReader(new File(_))

  private def calendarReader(pattern: String): Reader[Calendar] = calendarReader(pattern, Locale.getDefault)
  private def calendarReader(pattern: String, locale: Locale): Reader[Calendar] =
    toReader { s =>
      val fmt = new SimpleDateFormat(pattern)
      val c = new GregorianCalendar
      c.setTime(fmt.parse(s))
      c
    }


  implicit def tupleRead[A: Reader, B: Reader]: Reader[(A, B)] = toReader(s => {
    val a = tupleDelim.split(s)
    implicitly[Reader[A]].read(tupleStart.replaceAllIn(a.head, _.group(1))) ->
      implicitly[Reader[B]].read(tupleEnd.replaceAllIn(a.last, _.group(1)))
  })

  implicit def listRead[S: Reader]: Reader[List[S]] =
    toReader[List[S]](s => commaDelim.split(s).map(implicitly[Reader[S]].read).toList)

  object ops {
    implicit class pp[T](s: String) {

      def read[T : Reader] = implicitly[Reader[T]].read(s)
    }
  }
}

//object Demo {
//
//  //def translate[T](s: String)(implicit ev: Reader[T]): T = ev.read(s)
//  def translate[Seq[A], A](s: String)(implicit ev: Reader[Seq[A]]) = ev.read(s)
//  def translate[List[A], A](s: String)(implicit ev: Reader[Seq[A]]) = ev.read(s).toList
//}

object Main {

  import Reader.ops._

  val sequence = "1, 2, 3, 4, 5"
  val seqOfTuples = "(one | 1), (two|  2), (three |3)"
  val date = "2016-02-12"

  def main(args: Array[String]): Unit = {
    import Adaptor._

    println(Reader[List[Int]].read(sequence).mkString(", "))
    println(Reader[List[(String,Int)]].read(seqOfTuples).mkString(", "))
    println(Reader[Calendar].read(date).getTime)

    println("true".read[Boolean])

    /////////////////////

    println(translateUsing(sequence, DoubleListReader).mkString(", "))

  }
}
