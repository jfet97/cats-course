package part2abstractMath

import scala.annotation.tailrec

object CustomMonads {

  import cats.Monad

  // custom monad instance for options
  given OptionMonad: Monad[Option] with {
    override def pure[A](x: A): Option[A] = Option(x)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)

    // it must be implemented because monads represent a ssequence of actions
    // and a sort of immutable iteration must be made available for methods like:
    // OptionMonad.iterateUntil, OptionMonad.iterateWhile, OptionMonad.iterateForever
    //
    // tailRecM should run again and again until the result of f is None or Option[Right(B)]
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] =
      f(a) match {
        case None => None
        case Some(Left(va)) =>
          tailRecM(va)(f) // tries again with the new A value
        case Some(Right(vb)) => Some(vb)
      }
    // tailrecM MUST NOT stack-overflow, it is a design contract

  }

  //
  //
  // TODO 1: define a monad for the identity type
  type Identity[T] = T
  val aNumber: Identity[Int] = 42

  given IdentityMonad: Monad[Identity] with {
    override def pure[A](a: A): Identity[A] = a

    override def flatMap[A, B](fa: Identity[A])(
        f: A => Identity[B]
    ): Identity[B] = f(fa)

    override def tailRecM[A, B](
        a: A
    )(f: A => Identity[Either[A, B]]): Identity[B] =
      f(a) match {
        case Left(va) =>
          tailRecM(va)(f) // tries again with the new A value
        case Right(vb) => vb
      }
  }

  //
  //
  // harder example
  sealed trait Tree[+A]
  final case class Leaf[+A](value: A) extends Tree[A]
  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // TODO 2: define a monad for this Tree
  given TreeMonad: Monad[Tree] with {
    override def pure[A](a: A): Tree[A] = Leaf(a)

    override def flatMap[A, B](fa: Tree[A])(
        f: A => Tree[B]
    ): Tree[B] = fa match {
      case Leaf(a)        => f(a)
      case Branch(lt, rt) => Branch(flatMap(lt)(f), flatMap(rt)(f))
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def stackRec(t: Tree[Either[A, B]]): Tree[B] = t match {
        // f has failed on some leafs, returning a Left(A) => try again with the new A
        case Leaf(Left(v)) => stackRec(f(v))
        // f was successful, returning a Right(B) => return a new Leaf(B)
        case Leaf(Right(b)) => Leaf(b)
        // induction step
        case Branch(left, right) => Branch(stackRec(left), stackRec(right))
      }

      //
      // todo: nodes (branches) not yet expanded (processed)
      // expanded: already processed
      // done: the accumulator for the result
      @tailrec
      def tailRec(
          todo: List[Tree[Either[A, B]]],
          expanded: Set[Tree[Either[A, B]]],
          done: List[Tree[B]]
      ): Tree[B] =
        if todo.isEmpty then done.head
        // pick a node from the todo list
        else
          todo.head match {
            // tries again with the new leaf'value, discard the failed current leaf
            case Leaf(Left(va)) => tailRec(f(va) :: todo.tail, expanded, done)
            // save the current leaf because it is good
            case Leaf(Right(vb)) =>
              tailRec(todo.tail, expanded, Leaf(vb) :: done)
            case node @ Branch(left, right) =>
              // new branch
              if (!expanded.contains(node)) then
                tailRec(right :: left :: todo, expanded + node, done)
              else {
                // already visited branch (recursion has ended on its sub-branches)
                // => cosntruct a new branch with the recursive results
                val newLeft = done.head
                val newRight = done.tail.head
                val newBranch = Branch(newLeft, newRight)
                tailRec(todo.tail, expanded, newBranch :: done.drop(2))
              }

          }

      tailRec(List(f(a)), Set(), List())

      /*
            _____1_____
         __2__       __3__
        /     \     /     \
       L1     R2   R3     R4
        tr([1], [], []) =
        tr([3, 2, 1], [1], []) =
        tr([R4, R3, 3, 2, 1], [3, 1], []) =
        tr([R3, 3, 2, 1], [3, 1], [B4]) =
        tr([3, 2, 1], [3, 1], [B3, B4]) =
        tr([2, 1], [1], [B34]) =
        tr([R2, L1, 2, 1], [2, 1], [B34]) =
        tr([L1, 2, 1], [2, 1], [B2, B34]) =
        tr([R1, 2, 1], [2, 1], [B2, B34]) =
        tr([2,1], [2, 1], [B1, B2, B34]) =
        tr([1], [1], [B12, B34]) =
        tr([], [], [B1234]) =
        B1234
       */
    }
  }

  def main(args: Array[String]): Unit = {

    val example: Tree[Either[Int, String]] =
      Branch(
        Branch(Leaf(Left(1)), Leaf(Left(2))),
        Branch(Leaf(Left(1)), Leaf(Left(2)))
      )
    def fun(x: Int): Tree[Either[Int, String]] =
      if (x == 0) example
      else Leaf(Right((x * 10).toString))

    print(Monad[Tree].tailRecM(0)(fun))

  }
}
