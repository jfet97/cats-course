package part4typeclasses

import java.util.concurrent.Executors
import cats.{Applicative, Foldable, Functor}
import scala.concurrent.{ExecutionContext, Future}

object Traversing {

  // high level approach to iteration

  given ec: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val servers: List[String] = List(
    "server-ci.rockthejvm.com",
    "server-staging.rockthejvm.com",
    "prod.rockthejvm.com"
  )
  def getBandwidth(hostname: String): Future[Int] = Future(hostname.length * 80)

  //
  /*
    we have
      - a List[String]
      - a func String => Future[Int]
    we want a Future[List[Int]]
   */
  val allBandwidths: Future[List[Int]] =
    servers.foldLeft(Future(List.empty[Int])) { (accumulator, hostname) =>
      val bandFuture: Future[Int] = getBandwidth(hostname)
      for {
        accBandwidths <- accumulator
        band <- bandFuture
      } yield accBandwidths :+ band // put band at the end
    }

  // the same: traverse the servers list, apply getBandwidth to each of them, then swap the containers
  val allBandwidthsTraverse: Future[List[Int]] =
    Future.traverse(servers)(getBandwidth)
  val allBandwidthsSequence: Future[List[Int]] =
    Future.sequence(servers.map(getBandwidth))

  //
  // TODO 1
  import cats.syntax.applicative._ // pure
  import cats.syntax.flatMap._ // flatMap
  import cats.syntax.functor._ // map
  import cats.syntax.apply._ // mapN
  def listTraverse[F[_]: Applicative, A, B](list: List[A])(
      func: A => F[B]
  ): F[List[B]] =
    // List.empty[B].pure[F] is a list of B "inside" F
    list.map(func).foldRight(List.empty[B].pure[F]) { (value, acc) =>
      ((lb: List[B]) => (b: B) => b :: lb).pure[F].ap(acc).ap(value)
    }

  //
  // TODO 2
  def listSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] =
    listTraverse(list)(identity)

  //
  // TODO 3 - what's the result of
  import cats.instances.vector._
  val allPairs = listSequence(List(Vector(1, 2), Vector(3, 4)))
  // Vector(List(1, 3), List(2, 3), List(1, 4), List(2, 4)
  val allTriples = listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))
  // Vector(List(1, 3, 5), List(2, 3, 5), List(1, 4, 5), List(2, 4, 5), List(1, 3, 6), List(2, 3, 6), List(1, 4, 6), List(2, 4, 6))

  // what does happen?
  // Vector acts like a list with respect of the ap method
  // The wrapped function acts on each element separately, then the multiple results
  // are combined into a Vector. If those results are functions as well, then each
  // function will be applied on each value contained into the next "apped" Vector

  println(allPairs)
  println(allTriples)

  //
  // TODO 4 - what's the result of (for-all)
  import cats.instances.option._
  def filterAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] =
    listTraverse[Option, Int, Int](list)(n => Some(n).filter(predicate))
  val allTrue = filterAsOption(List(2, 4, 6))(_ % 2 == 0) // Some(List(2,4,6))
  val someFalse = filterAsOption(List(1, 2, 3))(_ % 2 == 0) // None
  // because the list becames List(None, Some(2), None) and None combine Some = None

  //
  // with applicative instances
  import cats.data.Validated
  import cats.instances.list._ // Semigroup[List] => Applicative[ErrorsOr]
  type ErrorsOr[T] = Validated[List[String], T]
  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorsOr[List[Int]] =
    listTraverse[ErrorsOr, Int, Int](list) { n =>
      if (predicate(n)) Validated.valid(n)
      else Validated.invalid(List(s"predicate for $n failed"))
    }

  // TODO 5 - what's the result of
  val allTrueValidated = filterAsValidated(List(2, 4, 6))(_ % 2 == 0) // Valid(List(2,4,6))
  val someFalseValidated = filterAsValidated(List(1, 2, 3))(_ % 2 == 0)
  // Invalid(List("predicate for 1", "predicate for 3"))

  println(someFalseValidated)

  //
  // Traverse
  trait MyTraverse[L[_]] extends Foldable[L] with Functor[L] {
    def traverse[F[_]: Applicative, A, B](container: L[A])(func: A => F[B]): F[L[B]]
    def sequence[F[_]: Applicative, A](container: L[F[A]]): F[L[A]] =
      traverse(container)(identity)

    // TODO 6: redefine map using traverse
    import cats.Id // Applicative[Id] is automatically constructed
    def map[A, B](wa: L[A])(f: A => B): L[B] =
      traverse[Id, A, B](wa)(f)
    // A => B === A => Id[B]
  }

  import cats.Traverse
  import cats.instances.future._ // Applicative[Future]
  val allBandwidthsCats = Traverse[List].traverse(servers)(getBandwidth)
  // Traverse[List]: the implicit list's instance is now in scope

  // extension methods
  import cats.syntax.traverse._ // sequence + traverse
  val allBandwidthsCats2 = servers.traverse(getBandwidth)
  // each element of server is mapped with servers, then traverse does its job

  //
  //
  def main(args: Array[String]): Unit = {}
}
