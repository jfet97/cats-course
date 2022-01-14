package part2abstractMath

import cats.Semigroup
import cats.instances.int._
import cats.instances.string._
import cats.instances.option._

// Semigroups COMBINE elements of the same type
object Semigroups {

  val naturalIntSemigroup =
    Semigroup[Int] // it uses the implicit above imported
  val intCombination = naturalIntSemigroup.combine(2, 46) // sum

  val naturalStringSemigroup = Semigroup[String]
  val stringCombination =
    naturalStringSemigroup.combine("I love ", "Cats") // concat

  // the combine function can acts as a reducer
  def reduceInt(list: List[Int]): Unit = {
    list.reduce(naturalIntSemigroup.combine)
  }

  // general API to reduce
  def reduceThings[T](list: List[T])(using semigroup: Semigroup[T]): T =
    list.reduce(semigroup.combine)

  val strings = List("test ", "the ", "general reduce ", "method")
  val numbers = List(1, 2, 3, 4, 5)

  //
  // TODO 1: support a new type creating a Semigroup instance
  case class Expense(id: Long, amount: Double)
  given expenseSemigroup: Semigroup[Expense] = Semigroup.instance {
    (ex1, ex2) =>
      Expense(Math.max(ex1.id, ex2.id), ex1.amount + ex2.amount)
  }

  val expenses = List(Expense(123, 10.1), Expense(456, 14.3), Expense(234, 5.6))

  //
  // extension methods from Semigroup - |+| (combine)
  import cats.syntax.semigroup._
  val anIntSum = 2 |+| 3 // uses the given Semigroup instance for Int
  val aStringConcat = "we like " |+| "semigroups"
  val aCombinedExpense = Expense(4, 83.2) |+| Expense(56, 46.8)

  //
  // TODO 2: implement reduceThins2 with the |+|
  def reduceThings2[T : Semigroup](list: List[T]): T =
    list.reduce(_ |+| _)


  //
  def main(args: Array[String]): Unit = {
    println(intCombination)
    println(stringCombination)

    println(reduceThings(strings))
    println(reduceThings(numbers))

    // compiler will produce an implicit Semigroup[Option[Int]] because there is a Semigroup[Int] in scope
    // same for Semigroup[Option[String]]
    // same for any type with an implicit Semigroup
    println(
      reduceThings(numbers.map(Option.apply))
    ) // an Option[Int] containing the sum of all the numbers

    println(reduceThings(expenses))
  }
}
