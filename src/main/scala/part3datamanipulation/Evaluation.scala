package part3datamanipulation

object Evaluation {
  /*
    Cats makes the distinction between
    - evaluating an expression eagerly
    - evaluating lazily and every time you request it
    - evaluating lazily and keeping the value (memoizing)
   */

  // eval is wrapper over some value
  import cats.Eval

  val instantEval = Eval.now {
    println("computing now!") // instantly evaluated
    12345
  }

  val redoEval = Eval.always {
    println("computing again!")
    23456
  }

  val delayedEval = Eval.later {
    println("computing later!")
    45678
  }

  //
  // pure manipulation: computation performed in f is always lazy, even when called on an eager (Now) instance
  val composedEvaluation = instantEval.flatMap(value1 => delayedEval.map(_ + value1))
  val anotherComposedEvaluation = for {
    value1 <- instantEval
    value2 <- delayedEval
  } yield value1 + value2 // identical

  //
  // TODO 1: predict the output
  val evalEx1 = for {
    a <- delayedEval
    b <- redoEval
    c <- instantEval
    d <- redoEval
  } yield a + b + c + d
  // now, later, again, again, sum, again, again, sum

  //
  // "remember" a computed value
  val dontRecompute = redoEval.memoize

  val tutorial = Eval
    .always { println("Step 1..."); "put the guitar on your lap" }
    .map { step1 =>
      println("Step 2"); s"$step1 then put your left hand on the neck"
    }
    .memoize // remember the value up to this point
    .map { steps12 =>
      println("Step 3, more complicated"); s"$steps12 then with the right hand strike the strings"
    }

  //
  // TODO 2: implement defer such that defer(Eval.now) does NOT run the side effects
  def defer[T](eval: => Eval[T]): Eval[T] =
    Eval.later(()).flatMap(_ => eval)

  //
  // TODO 3: rewrite the method with Evals
  def reverseList[T](list: List[T]): List[T] =
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head

    // it is stack safe because of the implementation of Eval
  def reverseEval[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.now(list)
    else Eval.defer(reverseEval(list.tail).map(_ :+ list.head))

  //
  //
  def main(args: Array[String]): Unit = {
    // println(instantEval.value)

    // redoEval.value // "computing again!" printed
    // redoEval.value // "computing again!" printed again

    // delayedEval.value // "computing later!" printed
    // delayedEval.value // nothing happens

    // the delayed evaluation needs to be evaluated now
    // but wouldn't be evaluated because of the flatMap itself
    // println(composedEvaluation.value)

    // evalEx1.value
    // evalEx1.value
    // now, later, again, again, sum, again, again, sum

    // "Now!" not printed
    println(defer(Eval.now {
      println("Now!")
      42
    }))

    // "Now!" is printed
    println(defer(Eval.now {
      println("Now!")
      42
    }).value)

    // stack safe
    reverseEval((1 to 10000).toList).value
    println("reversed")

  }
}
