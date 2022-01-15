package part5alien

import cats.Monoid

object InvariantFunctors {

  // from A to string and viceversa
  trait Crypto[A] { self =>
    def encrypt(value: A): String
    def decrypt(encrypted: String): A

    def imap[B](back: B => A, forth: A => B): Crypto[B] = new Crypto[B] {
      override def encrypt(value: B) = self.encrypt(back(value))
      override def decrypt(encrypted: String) = forth(self.decrypt(encrypted))
    }
  }

  // API methods (wrappers over the trait's API)
  def encrypt[A](value: A)(using crypto: Crypto[A]): String = crypto.encrypt(value)
  def decrypt[A](repr: String)(using crypto: Crypto[A]): A = crypto.decrypt(repr)

  //
  // a Cypher[String] instance
  given caesarCypher: Crypto[String] with {
    override def encrypt(value: String) = value.map(c => (c + 2).toChar)
    override def decrypt(encrypted: String) = encrypted.map(c => (c - 2).toChar)
  }

  /*
    How can we support ints, doubles, Option[String] using caesarCypher?
    We need a conversion function from T to String and viceversa: the imap method
   */

  given doubleCrypto: Crypto[Double] = caesarCypher.imap(_.toString, _.toDouble)

  // TODO 1 - support Option[String]
  given optionStringCrypto: Crypto[Option[String]] = caesarCypher.imap(_.getOrElse(""), Option(_))

  // TODO 2 - Crypto[T] => Crypto[Option[T]], if you have a Monoid[T] in scope
  given optionCrypto[T](using crypto: Crypto[T])(using monoid: Monoid[T]): Crypto[Option[T]] =
    crypto.imap(_.getOrElse(monoid.empty), Option(_))

  //
  // Cats
  import cats.Invariant
  import cats.Show
  import cats.instances.string._ // Show[String]
  val showString = Show[String]
  val showOptionString: Show[Option[String]] = Invariant[Show].imap(showString)(Option(_))(_.getOrElse(""))
  // from and to Show[String]

  import cats.syntax.invariant._
  val showOptionString2: Show[Option[String]] = showString.imap(Option(_))(_.getOrElse("")) // identical

  //
  // Traits
  // TODO - what's the relationship?
  // if A extends B then A is stronger than B (B methods can be implemented by A)
  // by using the identity function I think you can implement map and contramap in terms of imap0
  trait MyInvariant[W[_]] {
    def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B]
  }

  trait MyContravariant[W[_]] extends MyInvariant[W] { // contravariant functor
    def contramap[A, B](wa: W[A])(back: B => A): W[B]

    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A) =
      contramap(wa)(back)
      // ignores forth
  }

  trait MyFunctor[W[_]] extends MyInvariant[W] { // covariant functor
    def map[A, B](wa: W[A])(forth: A => B): W[B]

    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A) =
      map(wa)(forth)
      // ignores back
  }

  //
  //
  def main(args: Array[String]): Unit = {
    val encrypted = encrypt("Let's encrypt")
    val decrypted = decrypt[String](encrypted)
    println(encrypted)
    println(decrypted)

    println(encrypt(Math.PI))
    println(decrypt[Double](encrypt(Math.PI)))
    println(encrypt(Option("Let's encrypt")))
    println(decrypt[Option[String]](encrypted))
    println(encrypt(Option(Math.PI)))
    println(decrypt[Option[Double]](encrypt(Math.PI)))

  }
}
