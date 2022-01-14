package part4typeclasses

import cats.Monad
import scala.util.Try
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import scala.concurrent.Future
import cats.Applicative

object HandlingErrors {

  trait MyApplicativeError[M[_], E] extends Applicative[M] {
    def raiseError[A](e: E): M[A]
    def handleErrorWith[A](ma: M[A])(func: E => M[A]): M[A]
    def handleError[A](ma: M[A])(func: E => A): M[A] = handleErrorWith(ma)(e => pure(func(e)))
  }

  trait MyMonadError[M[_], E] extends Monad[M] {
    def ensure[A](ma: M[A])(error: E)(predicate: A => Boolean): M[A]
  }

  import cats.MonadError
  import cats.instances.either._ // implicit MonadError
  type ErrorOr[A] = Either[String, A]
  val monadErrorEither = MonadError[ErrorOr, String] // must use String here

  // methods from Monad + raiseError
  val success = monadErrorEither.pure(32) // Either[String, Int] == Right(32)
  val failure = monadErrorEither.raiseError[Int]("something wrong") // Either[String, Int] == Left("something wrong")

  // handle an error
  val handledError = monadErrorEither.handleError(failure) {
    case "Badness" => 44
    case _         => 89
  }
  val handledError2 = monadErrorEither.handleErrorWith(failure) {
    case "Badness" => monadErrorEither.pure(44)
    case _         => Left("Something else")
  }

  // filter
  val filteredSccess = monadErrorEither.ensure(success)("Number too small")(_ > 100)

  //
  // other monad errors

  // Try and Future
  import cats.instances.try_._ // implicit MonadError[Try] where E = Throwable

  val exception = new RuntimeException("Really bad")
  val pureException = MonadError[Try, Throwable].raiseError(exception) // Failure(exception)

  given ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  MonadError[Future, Throwable].raiseError(exception) // Future which will complete with a Failure(exception)

  // applictives => ApplicativeError
  import cats.data.Validated // is an example
  type ErrorsOr[T] = Validated[List[String], T]

  import cats.ApplicativeError
  val applErrorVal = ApplicativeError[ErrorsOr, List[String]] // implicit Semigroup[List]
  // we have: pure, raiseError, handleError, handleErrorWith

  //
  // extension methods
  import cats.syntax.applicative._ // pure
  import cats.syntax.applicativeError._ // raiseError, handleError(With)
  val extendedSuccess = 42.pure[ErrorsOr] // requires the implicit ApplicativeError[ErrorsOr, List[String]]
  val extendedError: ErrorsOr[Int] = List("Badness").raiseError[ErrorsOr, Int]
  val recoveredError: ErrorsOr[Int] = extendedError.recover { case _ =>
    43
  }

  import cats.syntax.monadError._ // ensure
  val testedSuccess = success.ensure("Something bad")(_ > 100)

  def main(args: Array[String]): Unit = {}
}
