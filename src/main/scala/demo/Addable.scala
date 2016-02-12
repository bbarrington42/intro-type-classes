package demo

/**
  * Created by bbarrington on 2/10/16.
  */
trait Addable[T] {
  def zero: T
  def add(t1: T, t2: T): T
}

object Addable {

  implicit def intAddable = new Addable[Int] {
    override def zero: Int = 0

    override def add(t1: Int, t2: Int): Int = t1 + t2
  }

  implicit def stringAddable = new Addable[String] {
    override def zero: String = ""

    override def add(t1: String, t2: String): String = t1 + t2
  }

  def sum[T](l: List[T])(implicit ev: Addable[T]) =
    l.foldLeft(ev.zero)(ev.add)
}

object Sum {
  def main(args: Array[String]): Unit = {
    import Addable._

    val result = sum(List(1,2,3,4,5))

    println(result)
  }
}
