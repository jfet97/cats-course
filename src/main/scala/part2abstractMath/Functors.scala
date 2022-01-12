package part2abstractMath

import scala.util.Try

object Functors {

  // mappables
  val aModifiedList = List(1, 2, 3).map(_ + 1)
  val aModifiedOption = Option(2).map(_ + 1)
  val aModifiedTry = Try(42).map(_ + 1)

  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  // the Cats Functor
  import cats.Functor

  import cats.instances.list._ // includes Functor[List]
  val listFunctor = Functor[List]
  val incrementedNumbers = listFunctor.map(aModifiedList)(_ + 1)

  import cats.instances.option._
  val optionFunctor = Functor[Option]
  val incrementedOption = optionFunctor.map(aModifiedOption)(_ + 1)

  import cats.instances.try_._
  val tryFunctor = Functor[Try]
  val incrementedTry = tryFunctor.map(aModifiedTry)(_ + 1)

  //
  // generalizing an API
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)
  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)
  def do10xTry(attempt: Try[Int]): Try[Int] = attempt.map(_ * 10)
  //
  def do10x[F[_]](container: F[Int])(using functor: Functor[F]): F[Int] = {
    // functor is the functor instance of F
    // F is a generic type costructor, it will be preserved
    functor.map(container)(_ * 10)
  }
  println(do10x(List(1, 2, 3)))

  //
  // TODO 1
  trait Tree[+T]
  object Tree {
    // "smart" constructors - IMPORTANT: the return type is a Tree[T]
    def leaf[T](value: T): Tree[T] = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] =
      Branch(value, left, right)
  }
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  // define an object which extends Functor[Tree]
  given functorTree: Functor[Tree] with {
    override def map[A, B](ta: Tree[A])(f: A => B) = ta match {
      case Leaf(v)           => Leaf(f(v))
      case Branch(v, la, ra) => Branch(f(v), map(la)(f), map(ra)(f))
    }
  }

  //
  // extension method - map
  import cats.syntax.functor._ // import extension methods
  val tree: Tree[Int] =
    Tree.branch(40, Tree.branch(5, Tree.leaf(10), Tree.leaf(30)), Tree.leaf(20))
  // map available because of the extension method map
  //(a functor instance is needed in scope)
  val incrementedTree = tree.map(_ + 1)

  //
  // TODO 2: write a shorted do10x method using extension methods
  def do10xShorter[F[_]: Functor](container: F[Int]): F[Int] =
    container.map(_ * 10)

  def main(args: Array[String]): Unit = {

    // Error: Branch(30, Tree.leaf(10), Tree.leaf(20)) is of type Branch[Int]
    // and because Tree is covariant, the compiler is not going to use the
    // Functor[Tree] instance (Cats' TCs are invariant)
    // println(do10x(Branch(30, Tree.leaf(10), Tree.leaf(20))))

    // sol. 1 (explicit the Tree type)
    println(do10x[Tree](Branch(30, Tree.leaf(10), Tree.leaf(20))))

    // sol. 2 (smart constructors)
    println(do10x(Tree.branch(30, Tree.leaf(10), Tree.leaf(20))))
  }
}
