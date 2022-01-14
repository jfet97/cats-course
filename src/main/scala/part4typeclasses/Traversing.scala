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
  //
  def main(args: Array[String]): Unit = {}
}
