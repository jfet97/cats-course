package part3datamanipulation

import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import scala.concurrent.Future

object Writers {

  import cats.data.Writer // A => B where B is fixed
  // to keep track of useful information while the data is being manipulated

  // Pattern:
  // 1 - define them at the start Writer[Logs type, Value type (wrapped)]
  val aWriter: Writer[List[String], Int] = Writer(List("Started something"), 45)

  // 2 - manipulate them with pure FP
  val anIncreasedWriter =
    // value increases, logs stay the same
    aWriter.map(_ + 1)

  val aLogsWriter =
    // value stays the same, logs change
    aWriter.mapWritten(
      _ :+ "found something interesting"
    )

  val aWriterWithBoth =
    // both value and logs change
    aWriter.bimap(
      _ :+ "found something interesting",
      _ + 1
    )
  val aWriterWithBoth2 = aWriter.mapBoth { (logs, value) =>
    // I could include the previous writer into the log
    (logs :+ "found something interesting", value + 1)
  }

  // 3 - dump either the value or the logs
  val desiredValue = aWriter.value
  val logs = aWriter.written
  val (l, v) = aWriter.run
  println(desiredValue)
  println(logs)

  //
  //
  // flatMap
  import cats.instances.vector._ // imports a Semigroup[Vector]
  val writerA = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB = Writer(Vector("Log B1"), 40)
  val compositeWriter = for {
    va <- writerA
    vb <- writerB
    // these logs will be combined by their natural combination function in the presence of a semigroup
  } yield va + vb
  println(compositeWriter.run)

  //
  // reset the logs
  import cats.instances.list._ // an implicit Monoid[List[Int]]
  val anEmptyWriter = aWriter.reset // clear the logs, keep the value,a monoid is needed

  //
  // TODO 1: rewrite a function which "prints" things with writers
  def countAndSay(n: Int): Unit = {
    if (n <= 0) println("starting!")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }

  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("starting!"), 0)
    else countAndLog(n - 1).flatMap(_ => Writer(Vector(s"$n"), n))
  }

  // Benefit #1: we work with pure FP: no side effects

  //
  // TODO 2: rewrite this method with writers
  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum(${n - 1}) = $lowerSum")
      lowerSum + n
    }
  }

  def sumWithLogs(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector(), 0)
    else
      for {
        // ingore the wrapped ints
        _ <- Writer(Vector(s"Now at $n"), n)
        lowerSum <- sumWithLogs(n - 1)
        _ <- Writer(Vector(s"Computed sum(${n - 1}) = $lowerSum"), n)
      } yield lowerSum + n
  }

  // Benefit #2: Writers can keep logs separate on multiple threads because we collect them
  // instead of printing them as soon as possible

  given ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  def main(args: Array[String]): Unit = {

    // ex 1
    countAndSay(10)
    countAndLog(10).written.foreach(println)

    //
    // ex 2
    // bad because of side effects: outputs are interleaved
    // Future(naiveSum(100)).foreach(println)
    // Future(naiveSum(100)).foreach(println)

    val sumFuture1 = Future(sumWithLogs(100))
    val sumFuture2 = Future(sumWithLogs(100))
    val logs1 = sumFuture1.map(_.written).foreach(println) // logs from thread 1
    println("-------------------------------------------------")
    val logs2 = sumFuture2.map(_.written).foreach(println) // logs from thread 2

  }
}
