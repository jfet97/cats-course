package part4typeclasses

import cats.Monoid
import cats.Eval

object Folding {

  // TODO - implement all in terms of foldLeft & foldRight
  object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] =
      // to not reverse the output list
      list.foldRight(List.empty[B])((a, currentList) => f(a) :: currentList)

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
      //                                               vvv same as currentList.append(f(a))
      list.foldLeft(List.empty[B])((currentList, a) => currentList.foldRight(f(a))(_ :: _))

    def filter[A](list: List[A])(predicate: A => Boolean): List[A] =
      list.foldRight(List.empty[A])((a, currentList) => if predicate(a) then a :: currentList else currentList)

    def combineAll[A](list: List[A])(using monoid: Monoid[A]): A =
      list.foldLeft(monoid.empty)(monoid.combine)
  }

  // Foldable
  import cats.Foldable

  //
  // foldLeft
  import cats.instances.list._ // implicit Foldable[List]
  val sum = Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _) // 6

  import cats.instances.option._ // implicit Foldable[Option]
  val sumOption1 = Foldable[Option].foldLeft(Option.empty[Int], 30)(_ + _) // 3
  val sumOption2 = Foldable[Option].foldLeft(Option(42), 30)(_ + _) // 72

  //
  // foldRight is stack-safe regardless of your container
  // (because foldRight is usually implemented using stack recursion)
  val sumRight: Eval[Int] = Foldable[List].foldRight(List(1, 2, 3), Eval.now(0)) { (num, eval) =>
    eval.map(_ + num)
  }

  //
  // convenience
  import cats.instances.int._ // Monoid[Int]
  val anotherSum = Foldable[List].combineAll(List(1, 2, 3)) // implicit Monoid[Int]
  import cats.instances.string._
  val mappedConcat = Foldable[List].foldMap(List(1, 2, 3))(_.toString) // implicit Monoid[String] => "123"

  //
  // nesting: we can combine foldables
  import cats.instances.vector._
  val intsNested = List(Vector(1, 2, 3), Vector(4, 5, 6))
  println((Foldable[List] compose Foldable[Vector]).combineAll(intsNested)) // 21 = 1 + 2 + 3 + 4 + 5 + 6

  //
  // extension methods
  import cats.syntax.foldable._
  val sum3 = List(1, 2, 3).combineAll // requires Foldable[List], Monoid[Int]
  val mappedConcat2 = List(1, 2, 3).foldMap(_.toString) // requires Foldable[List], Monoid[String]

  //
  //
  println("main")
  def main(args: Array[String]): Unit = {
    import ListExercises._

    val numbers = (1 to 10).toList
    println(map(numbers)(_ + 1))
    println(flatMap(numbers)(x => (1 to x).toList))
    println(filter(numbers)(_ % 2 == 0))
    println(combineAll(numbers))
  }
}
