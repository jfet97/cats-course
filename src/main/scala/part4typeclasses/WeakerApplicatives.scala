package part4typeclasses

object WeakerApplicatives {
  import cats.{Functor, Semigroupal}

  // Apply: ap + Functor + Semigroupal
  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W] {
    override def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] = {
      // there is no pure
      ap(map(fa)((a: A) => (b: B) => (a, b)))(fb)
    }

    // TODO
    def mapN[A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] = {
      map(product(tuple._1, tuple._2))(t => f(t._1, t._2))
    }

    def ap[B, T](wf: W[B => T])(wb: W[B]): W[T] // fundamental
  }

  // Applicative: pure + Apply
  trait MyApplicative[W[_]] extends MyApply[W] {
    def pure[A](s: A): W[A] // fundamental
  }

  //
  // example:
  import cats.Apply
  import cats.instances.option._ // implicit Apply[Option]
  val applyOption = Apply[Option]
  val funcApp = applyOption.ap(Some((x: Int) => x + 1))(Some(2)) // Some(3)

  //
  // extension methods from Apply
  import cats.syntax.apply._
  val tupleOfOptions = (Option(1), Option(2), Option(3))
  val optionOfTuple = tupleOfOptions.tupled // Some((1,2,3))
  val sumOption = tupleOfOptions.mapN(_ + _ + _) // Some(6)


  def main(args: Array[String]): Unit = {}
}
