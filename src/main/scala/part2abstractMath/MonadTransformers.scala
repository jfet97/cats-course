package part2abstractMath

import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import scala.concurrent.Future

object MonadTransformers {

  def sumAllOptions(values: List[Option[Int]]): Int = ???
  // monad transformers let us to avoid the manual unwrap of the inner monad
  // whatever outer monad is present

  import cats.data.OptionT
  import cats.instances.list._ // fetch an implicit OptionT[List]
  // OptionT[F[_], A] represent a value of type F[Option[A]]
  import cats.instances.future._

  // represents a List[Option[Int]]
  val listOfNumberOptions: OptionT[List, Int] = OptionT(
    List(Option(1), Option(2))
  )
  // represents a List[Option[Char]]
  val listOfCharsOptions: OptionT[List, Char] = OptionT(
    List(Option('a'), Option('b'), Option.empty)
  )

  // now the magic: OptionT[List, (Int, Char)] === List[Option[(Int, Char)]]
  val listOfTuples = for {
    number <- listOfNumberOptions
    char <- listOfCharsOptions
  } yield (number, char)

  //
  //
  import cats.instances.future._
  import cats.data.EitherT

  // List[Either[String, Int]]
  val listOfEithers: EitherT[List, String, Int] = EitherT(
    List(Left("something wrong"), Right(43), Right(2))
  )

  given ec: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  // Future[Either[String, Int]]
  val futureOfEither: EitherT[Future, String, Int] =
    // wrap over Future(Right(45)) that is not directly assignable
    EitherT.right(Future(45))

  //
  //
  /*
    TODO exercise
    We have a multi-machine cluster for your business which will receive a traffic surge following a media appearance.
    We measure bandwidth in units.
    We want to allocate TWO of our servers to cope with the traffic spike.
    We know the current capacity for each server and we know we'll hold the traffic if the sum of bandwidths is > 250.
   */
  val bandwidths = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170
  )
  // wrapper over Future[Either[String, T]] because the error
  // could happen asynchronously
  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandwidth(server: String): AsyncResponse[Int] =
    bandwidths.get(server) match {
      case None    => EitherT.left(Future(s"Server $server unreachable"))
      case Some(b) => EitherT.right(Future(b))
    }

  // TODO 1
  // hint: call getBandwidth twice, and combine the results

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
    band1 <- getBandwidth(s1)
    band2 <- getBandwidth(s2)
  } yield (band1 + band2 > 250)

  // TODO 2
  // hint: call canWithstandSurge + transform
  def generateTrafficSpikeReport(
      s1: String,
      s2: String
  ): AsyncResponse[String] = canWithstandSurge(s1, s2).transform {
    // transform the inner Either
    case Left(reason) =>
      Left(s"Servers $s1 and $s2 CANNOT cope with the incoming spike: $reason")
    case Right(false) =>
      Left(
        s"Servers $s1 and $s2 CANNOT cope with the incoming spike: not enough total bandwidth"
      )
    case Right(true) =>
      Right(s"Servers $s1 and $s2 can cope with the incoming spike NO PROBLEM!")
  }
  // Either[A, B] => Either[C, D]: Future[Either[String, Boolean]] => Future[Either[String, String]]

  //
  //
  def main(args: Array[String]): Unit = {
    // List(Some((1,a)), Some((1,b)), None, Some((2,a)), Some((2,b)), None)
    println(listOfTuples.value)

    // EitherT[Future, String, String].value is something of type Future[Either[String, String]]
    val resultFuture = generateTrafficSpikeReport(
      "server2.rockthejvm.com",
      "server3.rockthejvm.com"
    ).value
    resultFuture.foreach(println)

  }
}
