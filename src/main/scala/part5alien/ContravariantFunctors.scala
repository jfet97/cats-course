package part5alien

import cats.kernel.Monoid

object ContravariantFunctors {

  trait Format[T] { self =>
    // contravariant type class (not in T, it is a contravariant functor)
    def format(value: T): String

    // the map acts "before": it transforms the A into a T, to then call format[T]
    // that is because Format is a consumer of Ts
    def contramap[A](func: A => T): Format[A] = new Format[A] {
      override def format(value: A) = self.format(func(value))
    }
  }

  def format[A](value: A)(using f: Format[A]) = f.format(value)

  // formatters
  given StringFormat: Format[String] with {
    override def format(value: String) = "\"" + value + "\""
  }
  given IntFormat: Format[Int] with {
    override def format(value: Int) = value.toString
  }
  given BooleanFormat: Format[Boolean] with {
    override def format(value: Boolean) = if (value) "YES" else "NO"
  }

  // problem: given Format[MyType], can we have a Format[Option[MyType]]?
  given getOptionFormat[T](using f: Format[T])(using m: Monoid[T]): Format[Option[T]] =
    f.contramap[Option[T]](_.getOrElse(m.empty))
  // if the option is a None, we still need a value of type T for the contramap

  //
  /*
    Map applies transformations in sequence
    Contramap applies transformations in REVERSE sequence
   */

  //
  // Cats
  import cats.Contravariant
  import cats.Show
  import cats.instances.int._ // implicit Show[Int]
  val showInts = Show[Int]
  val showOption: Show[Option[Int]] = Contravariant[Show].contramap(showInts)(_.getOrElse(0))
  // Contravariant[Show]: the contravariant instance of Show

  import cats.syntax.contravariant._
  val showOptionsShorter: Show[Option[Int]] = showInts.contramap(_.getOrElse(0))

  //
  //
  def main(args: Array[String]): Unit = {
    println(format("Nothing weird so far"))
    println(format(42))
    println(format(true))

    println(format(Option(42)))
    // import cats.instances.option._
    println(format(Option(Option(42))))
  }
}
