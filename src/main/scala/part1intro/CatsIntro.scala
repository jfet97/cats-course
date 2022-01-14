package part1intro

object CatsIntro {

  // Eq
  // val aComparison = 2 == "a string" // it does compile :( in Scala 2, not in Scala 3

  // part1 - type class import
  import cats.Eq

  // part 2 - import TC instances for the types you need
  import cats.instances.int._ // _ means al the TC of int

  // part 3 - use the TC API
  val intEquality = Eq[Int] // uses the implicit imported above
  val aTypeSafeComparison = intEquality.eqv(2, 3);
  // val anUnsafeComparison = intEquality.eqv(2, "string") -- does not compile

  // part 4 - use extension methods (if applicable)
  import cats.syntax.eq._ // _ means all the extension methods the typeclass does support
  val anothertypeSafeComp = 2 === 3 // uses the implicit imported above
  val neqComparison = 2 =!= 3
  // val invalidComparison = 2 === "a String" -- doesn't compile
  // extension methods are only visible in the presence of the right TC instance

  // part 5 - extending the TC operations to composite types, e.g. lists
  import cats.instances.list._ // we bring Eq[List[Int]] in scope
  val aListComparison = List(2) === List(2)

  // part 6 - create a TC for custom composite types
  final case class ToyCar(model: String, price: Double)
  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] { (c1, c2) =>
    c1.price == c2.price
  }

  def main(args: Array[String]): Unit = {}
}
