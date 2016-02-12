package demo

import java.io.File
import java.text.SimpleDateFormat
import java.util.{Calendar, GregorianCalendar, Locale}

import scala.annotation.implicitNotFound



@implicitNotFound("No member of type class Reader in scope for ${A}")
trait Reader[A] {
  def read(s: String): A
}



object Reader {

  val commaDelim = """\s*,\s*""".r
  val equalDelim = """\s*=\s*""".r
  val tupleDelim = """\)\s*,\s*\(""".r

  val tuples = """,?\(([^)]+\s*,\s*[^)]+)\)""".r

  val sequences = """""".r


  private def read[T](f: String => T): Reader[T] =
    new Reader[T] {
      def read(s: String) = f(s)
    }

  private def map[A,B](reader: Reader[A])(f: A => B): Reader[B] = new Reader[B] {
    override def read(s: String): B = f(reader.read(s))
  }

  implicit val intRead: Reader[Int] = read(_.toInt)

  implicit val doubleRead: Reader[Double] = read(_.toDouble)

  implicit val integerRead: Reader[Integer] = read(s => Integer.parseInt(s))

  implicit val stringRead: Reader[String] = read(identity)

  implicit val booleanRead: Reader[Boolean] = read(_.toBoolean)

  implicit val yyyymmdddRead: Reader[Calendar] = calendarReader("yyyy-MM-dd")

  implicit val fileRead: Reader[File] = read(new File(_))

  private def calendarReader(pattern: String): Reader[Calendar] = calendarReader(pattern, Locale.getDefault)
  private def calendarReader(pattern: String, locale: Locale): Reader[Calendar] =
    read { s =>
      val fmt = new SimpleDateFormat(pattern)
      val c = new GregorianCalendar
      c.setTime(fmt.parse(s))
      c
    }

  implicit def tupleRead[A: Reader, B: Reader]: Reader[(A, B)] = read(s => {
    val a = commaDelim.split(s)
    implicitly[Reader[A]].read(a.head) -> implicitly[Reader[B]].read(a.last)
  })

  implicit def seqTupleRead[A: Reader, B: Reader]: Reader[Seq[(A, B)]] = read(s =>
    tuples.findAllMatchIn(s).map(m => implicitly[Reader[(A, B)]].read(m.group(1))).toSeq)

  implicit def seqRead[A: Reader]: Reader[Seq[A]] =
    read(s => commaDelim.split(s).map(implicitly[Reader[A]].read).toSeq)


//  implicit def listRead[A: Reader]: Reader[List[A]] =
//    map(seqRead[A])(_.toList)

//  implicit def listTupleRead[A: Reader, B: Reader]: Reader[List[(A, B)]] =
//    map(seqTupleRead[A,B])(_.toList)
}

object Demo {

  //def translate[T](s: String)(implicit ev: Reader[T]): T = ev.read(s)
  def translate[Seq[A], A](s: String)(implicit ev: Reader[Seq[A]]) = ev.read(s)
  def translate[List[A], A](s: String)(implicit ev: Reader[Seq[A]]) = ev.read(s).toList
}

object Main {

  val sequence = "1, 2, 3, 4, 5"
  val seqOfTuples = "(one, 1), (two,  2), (three ,  3)"


  def main(args: Array[String]): Unit = {
    import Adaptor._
    import Demo._

    println(translate[List, Int](sequence).mkString(", "))

     println(translate[Seq, Double](sequence).mkString(", "))
//
//    println(translate[Seq[(String, Int)]](seqOfTuples).mkString(", "))


    /////////////////////

    println(translateUsing(sequence, DoubleListReader).mkString(", "))

  }
}
