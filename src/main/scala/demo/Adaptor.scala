package demo

import java.io.File

/**
  * Created by bbarrington on 2/10/16.
  */
object Adaptor {

  val commaDelim = """\s*,\s*""".r

  trait StringReader[T] {
    def read(s: String): T
  }

  case object IntReader extends StringReader[Int] {
    override def read(s: String): Int = s.toInt
  }

  case object DoubleReader extends StringReader[Double] {
    override def read(s: String): Double = s.toDouble
  }

  case object BooleanReader extends StringReader[Boolean] {
    override def read(s: String): Boolean = s.toBoolean
  }

  case object FileReader extends StringReader[File] {
    override def read(s: String): File = new File(s)
  }

  case object IntListReader extends StringReader[List[Int]] {
    override def read(s: String): List[Int] =
      commaDelim.split(s).map(IntReader.read).toList
  }

  case object DoubleListReader extends StringReader[List[Double]] {
    override def read(s: String): List[Double] =
      commaDelim.split(s).map(DoubleReader.read).toList
  }


  def translateUsing[A](s: String, reader: StringReader[A]): A =
    reader.read(s)

}

object DemoAdaptor {
  def main(args: Array[String]): Unit = {
    import Adaptor._

    println(translateUsing("1, 2, 3, 4, 5", DoubleListReader).mkString(", "))
  }
}
