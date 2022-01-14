package part2abstractMath

object Monoids {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._ // import the |+| extension method

  val numbers = (1 to 1000).toList
  // |+| is always associative
  val sumLeft = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)

  //
  // a general API
  /*
  def combineFold[T](list: List[T])(using s: Semigroup[T]): T =
    list.foldLeft(/* ? */)(_ |+| _)
   */
  // a semigroup is not enough: there is no a general starting (empty/zero) values and the semigroup
  // instances don't provide one

  // MONOIDS = semigroups + zero values
  import cats.Monoid
  val intMonoid = Monoid[Int]
  val combineInt = intMonoid.combine(intMonoid.empty, 42) // 42
  // intMonoid.empty is 0

  import cats.instances.option._ // construct an implicit Monoid[Option[Int]]
  val emptyOption = Monoid[Option[Int]].empty // None
  val combineOption = Monoid[Option[Int]].combine(Option(3), emptyOption)
  val combineOption2 = Monoid[Option[Int]].combine(Option(2), combineOption)
  println(combineOption2) // Some(5)

  //
  // extension methods as |+|: Monoid extends Semigroup, so the Semigroup one is enough
  // or import cats.syntax.monoid._
  val combinedOptionFancy = Option(3) |+| Option(7)

  //
  // TODO 1
  def combineFold[T](list: List[T])(using mo: Monoid[T]): T =
    list.foldLeft(mo.empty)(_ |+| _)

  println(combineFold((1 to 1000).toList)) // 500500

  //
  // TODO 2
  val phonebooks = List(
    Map(
      "Alice" -> 235,
      "Bob" -> 647
    ),
    Map(
      "Charlie" -> 372,
      "Daniel" -> 889
    ),
    Map(
      "Tina" -> 123
    )
  )
  import cats.instances.map._

  println(combineFold(phonebooks))

  //
  // TODO 3 - shopping cart and online stores with Monoids
  // hint: define your monoid - Monoid.instance
  // hint #2: use combineByFold
  case class ShoppingCart(items: List[String], total: Double)

  given shoppingCartMonoid: Monoid[ShoppingCart] =
    Monoid.instance(
      ShoppingCart(List.empty, 0.0),
      (sc1, sc2) => ShoppingCart(sc1.items ++ sc2.items, sc1.total + sc2.total)
    )

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart =
    combineFold(shoppingCarts)

  println(
    checkout(
      List(
        ShoppingCart(List("iphone", "shoes"), 799),
        ShoppingCart(List("TV"), 20000),
        shoppingCartMonoid.empty
      )
    )
  )

  def main(args: Array[String]): Unit = {}
}
