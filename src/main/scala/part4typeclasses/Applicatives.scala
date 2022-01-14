package part4typeclasses

object Applicatives {

  // Monads extend Applicatives
  // Applicatives extend Functors

  import cats.Applicative
  import cats.instances.list._

  // bring the list applicative in scope as a value
  val listApplicative = Applicative[List]
  // bring the option applicative in scope as a value
  val optionApplicative = Applicative[Option]

  //
  // pure
  val aList = listApplicative.pure(2) // List(2)
  val anOption = optionApplicative.pure(2) // Some(2)

  // pure extension method
  import cats.syntax.applicative._
  val aSweetList = 2.pure[List] // List(2)
  val aSweetOption = 2.pure[Option] // Some(2)

  // Validated is an applicative
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  // behave like the pure/map/ap methods of Validated
  val validatedApplicative = Applicative[ErrorsOr]

  //
  // TODO: thought experiment
  def productWithApplicatives[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] = {
    // def ap[W[_], B, T](wf: W[B => T])(wa: W[B]): W[T]
    applicative.ap(applicative.ap(applicative.pure((a: A) => (b: B) => (a, b)))(wa))(wb)
  }

  // Applicatives have this ap[W[_], B, T](wf: W[B => T])(wa: W[B]): W[T]
  // Applicatives can implement product from Semigroupal
  // => Applicative extends Semigroupal

  def main(args: Array[String]): Unit = {}
}
