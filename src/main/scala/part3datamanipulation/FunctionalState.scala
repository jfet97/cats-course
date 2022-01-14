package part3datamanipulation

object FunctionalState {

  // S: state, A: value obtained from the state after a computation step
  type MyState[S, A] = S => (S, A)

  import cats.data.State

  val countAndSay: State[Int, String] = State(currentCount => (currentCount + 1, s"Counted $currentCount"))
  val (eleven, counted10) = countAndSay.run(10).value // 10 is the initial state
  // state = "iterative" stateful computations

  // iterative
  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, obtained $a"
  a *= 5
  val secondComputation = s"Multiplied with 5, obtained $a"

  // pure FP with states
  val firstTransformation = State((s: Int) => (s + 1, s"Added 1 to 10 - obtained ${s + 1}"))
  val secondTransformation = State((s: Int) => (s * 5, s"Multiplied with 5 - obtained ${s * 5}"))
  // composing...
  val compositeTransformation: State[Int, (String, String)] = firstTransformation.flatMap { firstResult =>
    secondTransformation.map(secondResult => (firstResult, secondResult))
  }
  val compositeTransformation2 = for {
    firstResult <- firstTransformation
    secondResult <- secondTransformation
  } yield (firstResult, secondResult)

  println(compositeTransformation2.run(10).value)

  // to access both state and comp. values
  val customTransformation = State((s: Int) => (s * 10, s"Multiplied ${s} with 10"))
  val compositeTransformationAccessStateAndValue = for {
    logStr1 <- customTransformation
    secondResult <- State((sn: Int) => (sn + 1, s"${logStr1} and then added 1 to ${sn}"))
  } yield secondResult
  println(compositeTransformationAccessStateAndValue.run(10).value)

  //
  // function composition is clunky
  val func1 = (s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}")
  val func2 = (s: Int) => (s * 5, s"Multiplied with 5, obtained ${s * 5}")
  val compositeFunc = func1.andThen { case (newState, firstResult) =>
    (firstResult, func2(newState))
  }

  //
  // TODO 1: an online store
  case class ShoppingCart(items: List[String], total: Double)
  def addToCart(item: String, price: Double): State[ShoppingCart, Double] = State { cart =>
    (ShoppingCart(item :: cart.items, cart.total + price), price + cart.total)
  }

  //
  // TODO 2: pure mental gymnastics

  // returns a State data structure that, when run, will not change the state but will issue the value f(a)
  def inspect[A, B](f: A => B): State[A, B] = State(sa => (sa, f(sa)))
  // returns a State data structure that, when run, returns the value of that state and makes no changes
  def get[A]: State[A, A] = State(sa => (sa, sa))
  // returns a State data structure that, when run, returns Unit and sets the state to that value
  def set[A](value: A): State[A, Unit] = State(sa => (value, ()))
  // returns a State data structure that, when run, will return Unit and sets the state to f(state)
  def modify[A](f: A => A): State[A, Unit] = State(sa => (f(sa), ()))

  val danielsCart: State[ShoppingCart, Double] = for {
    // ignore intermediate total's valuess
    _ <- addToCart("Fender guitar", 500)
    _ <- addToCart("Elixir strings", 19)
    total <- addToCart("Electric cable", 8)
  } yield total

  println(danielsCart.run(ShoppingCart(List(), 0)).value)

  //
  // methods available
  import cats.data.State._

  // state used as value
  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int] // get the state as the value
    _ <- set[Int](a + 10) // set the state
    b <- get[Int] // get the new state as the value
    _ <- modify[Int](_ + 43) // apply the function to the "implicit" state
    c <- inspect[Int, Int](_ * 2) // return the state after applying a transformation
  } yield (a, b, c)

  //
  //
  def main(args: Array[String]): Unit = {}
}
