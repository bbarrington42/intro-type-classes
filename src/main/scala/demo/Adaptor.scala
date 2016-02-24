package demo

import java.io.File

/**
  * Created by bbarrington on 2/10/16.
  */
object Adaptor {

  val commaDelim = """\s*,\s*""".r

  case object IntReader extends Reader[Int] {
    override def read(s: String): Int = s.toInt
  }

  case object DoubleReader extends Reader[Double] {
    override def read(s: String): Double = s.toDouble
  }

  case object BooleanReader extends Reader[Boolean] {
    override def read(s: String): Boolean = s.toBoolean
  }

  case object FileReader extends Reader[File] {
    override def read(s: String): File = new File(s)
  }


  case object IntListReader extends Reader[List[Int]] {
    override def read(s: String): List[Int] =
      commaDelim.split(s).map(IntReader.read).toList
  }

  case object DoubleListReader extends Reader[List[Double]] {
    override def read(s: String): List[Double] =
      commaDelim.split(s).map(DoubleReader.read).toList
  }

  // TODO Is there a way?
//  def ListReader[S] = new Reader[List[S]] {
//    override def read(s: String): List[S] = {
//      commaDelim.split(s).map(str => translateUsing(str, Reader[S])).toList
//    }
//  }


  def translateUsing[A](s: String, reader: Reader[A]): A =
    reader.read(s)

}

object DemoAdaptor {
  def main(args: Array[String]): Unit = {
    import Adaptor._

    println(translateUsing("1, 2, 3, 4, 5", DoubleListReader).mkString(", "))
    //println(translateUsing("1, 2, 3, 4, 5", ListReader[Double]).mkString(", "))
  }
}
