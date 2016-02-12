package demo

object ReadSample extends App {

  /**
    * The readable trait defines how objects can be converted from a string
    * representation to the objects instance. For most of the standard types
    * we can simply use the toType function of String.
    */
  trait Readable[T] {
    def read(x: String): T
  }

  /**
    * Companion object containing helper functions and standard implementations
    */
  object Readable {

    /**
      * Helper function which allows creation of Readable instances
      */
    def toReadable[T](p: String => T): Readable[T] = new Readable[T] {
      def read(x: String): T = p(x)
    }

    /**
      * Allow for construction of standalone readables, if the ops aren't used
      */
    def apply[A](implicit instance: Readable[A]): Readable[A] = instance

    // Using the toReadable creates cleaner code, we could also explicitly
    // define the implicit instances:
    //
    //   implicit object ReadableDouble extends Readable[Double] {
    //      def read(s: String): Double = s.toDouble
    //    }
    //    implicit object ReadableInt extends Readable[Int] {
    //      def read(s: String): Int = s.toInt
    //    }
    implicit val ReadableDouble = toReadable[Double](_.toDouble)
    implicit val ReadableInt = toReadable[Int](_.toInt)
    implicit val ReadableLong = toReadable[Long](_.toLong)
    implicit val ReadableString = toReadable[String](identity)
    implicit val ReadableBoolean = toReadable[Boolean](_.toBoolean)
    implicit val ReadableCharList = toReadable[List[Char]](_.toCharArray.toList)
//    implicit val ReadableStringList = toReadable[List[String]](_.split(':').toList)

    // My additions
    implicit def ReadableList[S: Readable]: Readable[List[S]] = toReadable[List[S]](_.split(",").map(implicitly[Readable[S]].read).toList)

    /**
      * Extend the string object with a read function.
      */
    object ops {
      implicit class pp[T](s: String) {

        /**
          * The type parameter should have an implcit Readable in scope. Use
          * implicitly to access it and call the read function
          */
        def read[T: Readable]= implicitly[Readable[T]].read(s)
      }
    }
  }

  // to use it, import the standard classes and import the String operation.

  import Readable._
  import Readable.ops._

  // now we can just get an instance of a readable and call the read function
  // to parse a string to a specific type.
  println(Readable[Double].read("10"))
  println(Readable[Int].read("10"))
  println(Readable[String].read("Well duh!"))
  println(Readable[List[Char]].read("Well duh!"))
  println(Readable[List[String]].read("Using:A:Separator:to:split:a:String"))

  // we can also use the read function directly
  println("20".read[Double]);
  println("Using:A:Separator:to:split:a:String".read[List[Char]]);
  println("Using:A:Separator:to:split:a:String".read[List[String]]);

  // creating custom read function can be done without tying a case class
  // to the reads implementation. In the following sample assume we
  // serialize it to a string with | as separators:
  //     10|Title Text|Title Content
  case class Task(id: Long, title: String, content: String)

  // simple convert the incoming string to a Task
  implicit val readableTask = toReadable(
    _.split('|') match {
      case Array(id: String, title: String, content: String) => new Task(id.read[Long], title, content)
    }
  )

  println(Readable[Task].read("10|Title Text|Title Content"))
  println("20|Another title Text|Another title Content".read[Task])
}
