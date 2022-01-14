package part4typeclasses

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Semigroupals {

  // a semigroupal exposes a method to tuple values regardless of how they were computed
  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  //
  // cats
  import cats.Semigroupal

  // options
  import cats.instances.option._ // implicit Semigroupal[Option]
  val optionSemigroupal = Semigroupal[Option]
  val aTupledOption = optionSemigroupal.product(Some(123), Some("a string")) // Some((123, "a string"))
  val aNoneTupled = optionSemigroupal.product(Some(123), None) // None

  // futures
  import cats.instances.future._ // implicit Semigroupal[Future]
  given ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  // Future(("the meaning of life", 42))
  val aTupledFuture = Semigroupal[Future].product(Future("the meaning of life"), Future(42))

  // lists
  import cats.instances.list._ // Monad[List] (ok, Monads are Semigroupals)
  val aTupledList = Semigroupal[List].product(List(1, 2), List("a", "b"))
  println(aTupledList) // List((1,a), (1,b), (2,a), (2,b)) - that is a List[(Int, String)]

  //
  // TODO: implement product with monads
  import cats.Monad
  import cats.syntax.functor._ // for map (extension method applicable to a monad)
  import cats.syntax.flatMap._ // for flatMap (extension method applicable to a monad)
  def productWithMonads[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] = for {
    a <- fa
    b <- fb
  } yield (a, b)

  //

  /** Why are semigroupals useful if monads can do products? A monad product follows from a sequence of map and flatMaps
    * which obey the so called monads laws, that are there to impose some sequence of operations We might want to
    * combine values without imposing a sequence of evaluations
    */

  //
  // example: Validated
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal = Semigroupal[ErrorsOr]
  // ^^^ requires the implicit Semigroup[List[_]] and Semigroup[T] to combine errors and vali results
  val invalidsCombination = validatedSemigroupal.product(
    Validated.invalid(List("Something wrong", "something else wrong")),
    Validated.invalid(List("This can't be right"))
  )
  // needs the Semigroupal instance of ErrorsOr, that is based on the one of a List,
  // to combine errors
  println(invalidsCombination)

  type EitherErrorsOr[T] = Either[List[String], T]
  import cats.instances.either._ // implicit Monad[Either], that is ok to be used as Semigroupal for Eithers
  val eitherSemigroupal = Semigroupal[EitherErrorsOr]
  val eitherCombination = eitherSemigroupal.product( // in terms of map/flatMap
    Left(List("Something wrong", "something else wrong")),
    Left(List("This can't be right"))
  )
  println(eitherCombination) // short circuit to the first Left

  //
  // TODO 2: define a Semigroupal[List] which does a zip
  val zipListSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](listA: List[A], listB: List[B]) = listA.zip(listB)
  }

  def main(args: Array[String]): Unit = {
    println(zipListSemigroupal.product(List(1, 2), List("a", "b")))
  }
}
