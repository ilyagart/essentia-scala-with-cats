def parseInt(str: String): Option[Int] =
  scala.util.Try(str.toInt).toOption
def divide(a: Int, b: Int): Option[Int] =
  if (b == 0) None else Some(a / b)
def stringDivideBy(aStr: String, bStr: String): Option[Int] =
  parseInt(aStr).flatMap { aNum =>
    parseInt(bStr).flatMap { bNum =>
      divide(aNum, bNum)
    }
  }
// same but clearer
def stringDivideBy2(aStr: String, bStr: String): Option[Int] =
  for {
    aNum <- parseInt(aStr)
    bNum <- parseInt(bStr)
    ans <- divide(aNum, bNum)
  } yield ans

stringDivideBy2("10", "5")
//res0: Option[Int] = Some(2)
stringDivideBy2("6", "2")
// res1: Option[Int] = Some(3)
stringDivideBy2("6", "0")
// res2: Option[Int] = None
stringDivideBy2("6", "foo")
// res3: Option[Int] = None
stringDivideBy2("bar", "2")
// res4: Option[Int] = None

trait Monad[F[_]] {
  def pure[A](value: A): F[A]
  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
  def map[A, B](value: F[A])(func: A => B): F[B] =
    flatMap(value)(a => pure(func(a)))
}

import scala.language.higherKinds
import cats.Monad
import cats.instances.option._ // for Monad
import cats.instances.list._ // for Monad
val opt1 = Monad[Option].pure(3)
// opt1: Option[Int] = Some(3)
val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
// opt2: Option[Int] = Some(5)
val opt3 = Monad[Option].map(opt2)(a => 100 * a)
// opt3: Option[Int] = Some(500)
val list1 = Monad[List].pure(3)
// list1: List[Int] = List(3)
val list2 = Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10))
// list2: List[Int] = List(1, 10, 2, 20, 3, 30)
val list3 = Monad[List].map(list2)(a => a + 123)
// list3: List[Int] = List(124, 133, 125, 143, 126, 153)

import cats.instances.future._ // for Monad
import scala.concurrent._
import scala.concurrent.duration._
//val fm = Monad[Future]
// <console>:37: error: could not find implicit value for parameter
//instance: cats.Monad[scala.concurrent.Future]
// val fm = Monad[Future]
//               ^
import scala.concurrent.ExecutionContext.Implicits.global
val fm = Monad[Future]
// fm: cats.Monad[scala.concurrent.Future] = cats.instances.FutureInstances$$anon$1@6a65fe88
val future = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))
Await.result(future, 1.second)
// res3: Int = 3
/*
The syntax for monads comes from three places:
• cats.syntax.flatMap provides syntax for flatMap;
• cats.syntax.functor provides syntax for map;
• cats.syntax.applicative provides syntax for pure.
In practice it’s often easier to import everything in one go from
cats.implicits
   */

import cats.instances.option._ // for Monad
import cats.instances.list._ // for Monad
import cats.syntax.applicative._ // for pure
1.pure[Option]
// res4: Option[Int] = Some(1)
1.pure[List]
// res5: List[Int] = List(1)
Monad[List].pure(1)

import cats.syntax.functor._ // for map
import cats.syntax.flatMap._ // for flatMap
def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
  a.flatMap(x => b.map(y => x * x + y * y))
def sumSquare2[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] = {
  for {
    x <- a
    y <- b
  } yield x * x + y * y
}
sumSquare(Option(3), Option(4))
// res8: Option[Int] = Some(25)
sumSquare(List(1, 2, 3), List(4, 5))
// res9: List[Int] = List(17, 26, 20, 29, 25, 34)
sumSquare2(List(1, 2, 3), List(4, 5))

import cats.Id
sumSquare(3: Id[Int], 4: Id[Int])
sumSquare2(3: Id[Int], 4: Id[Int])

"Dave": Id[String]
// res3: cats.Id[String] = Dave
123: Id[Int]
// res4: cats.Id[Int] = 123
List(1, 2, 3): Id[List[Int]]
// res5: cats.Id[List[Int]] = List(1, 2, 3)

val a = Monad[Id].pure(3)
// a: cats.Id[Int] = 3
val b = Monad[Id].flatMap(a)(_ + 1)
// b: cats.Id[Int] = 4
for {
  x <- a
  y <- b
} yield x + y
// res6: cats.Id[Int] = 7

def pure[A](value: A): Id[A] = value
def map[A, B](initial: Id[A])(func: A => B): Id[B] = func(initial)
def flatMap[A, B](initial: Id[A])(func: A => Id[B]): Id[B] = func(initial)

pure(123)
map(123)(_ * 2)
flatMap(123)(_ * 2)

val either1: Either[String, Int] = Right(10)
val either2: Either[String, Int] = Right(32)
for {
  a <- either1
  b <- either2
} yield a + b

import cats.syntax.either._ // for asRight
val a = 3.asRight[String]
// a: Either[String,Int] = Right(3)
val b = 4.asRight[String]
// b: Either[String,Int] = Right(4)
for {
  x <- a
  y <- b
} yield x * x + y * y
// res4: scala.util.Either[String,Int] = Right(25)

def countPositive(nums: List[Int]) =
  nums.foldLeft(0.asRight[String]) { (accumulator, num) =>
    if (num > 0) {
      accumulator.map(_ + 1)
    } else {
      Left("Negative. Stopping!")
    }
  }
countPositive(List(1, 2, 3))
// res5: Either[String,Int] = Right(3)
countPositive(List(1, -2, 3))
// res6: Either[String,Int] = Left(Negative. Stopping!)
Either.catchOnly[NumberFormatException]("foo".toInt)
// res7: Either[NumberFormatException,Int] = Left(java.lang.NumberFormatException: For input string: "foo")
Either.catchNonFatal(sys.error("Badness"))
// res8: Either[Throwable,Nothing] = Left(java.lang.RuntimeException:Badness)
Either.fromTry(scala.util.Try("foo".toInt))
// res9: Either[Throwable,Int] = Left(java.lang.NumberFormatException:For input string: "foo")
Either.fromOption[String, Int](None, "Badness")
// res10: Either[String,Int] = Left(Badness)
"Error".asLeft[Int].getOrElse(0)
// res11: Int = 0
"Error".asLeft[Int].orElse(2.asRight[String])
// res12: Either[String,Int] = Right(2)
-1.asRight[String].ensure("Must be non-negative!")(_ > 0)
// res13: Either[String,Int] = Left(Must be non-negative!)
"error".asLeft[Int].recover {
  case str: String => -1
}
// res14: Either[String,Int] = Right(-1)
"error".asLeft[Int].recoverWith {
  case str: String => Right(-1)
}
// res15: Either[String,Int] = Right(-1)
"foo".asLeft[Int].leftMap(_.reverse)
// res16: Either[String,Int] = Left(oof)
6.asRight[String].bimap(_.reverse, _ * 7)
// res17: Either[String,Int] = Right(42)
"bar".asLeft[Int].bimap(_.reverse, _ * 7)
// res18: Either[String,Int] = Left(rab)
123.asRight[String]
// res19: Either[String,Int] = Right(123)
123.asRight[String].swap
// res20: scala.util.Either[Int,String] = Left(123)
for {
  a <- 1.asRight[String]
  b <- 0.asRight[String]
  c <- if (b == 0) "DIV0".asLeft[Int]
  else (a / b).asRight[String]
} yield c * 100
// res21: scala.util.Either[String,Int] = Left(DIV0)

sealed trait LoginError extends Product with Serializable

final case class UserNotFound(username: String) extends LoginError

final case class PasswordIncorrect(username: String) extends LoginError

case object UnexpectedError extends LoginError

case class User(username: String, password: String)

type LoginResult = Either[LoginError, User]

// Choose error-handling behaviour based on type:
def handleError(error: LoginError): Unit =
  error match {
    case UserNotFound(u) =>
      println(s"User not found: $u")
    case PasswordIncorrect(u) =>
      println(s"Password incorrect: $u")
    case UnexpectedError =>
      println(s"Unexpected error")
  }
val result1: LoginResult = User("dave", "passw0rd").asRight
// result1: LoginResult = Right(User(dave,passw0rd))
val result2: LoginResult = UserNotFound("dave").asLeft
// result2: LoginResult = Left(UserNotFound(dave))
result1.fold(handleError, println)
// User(dave,passw0rd)
result2.fold(handleError, println)
// User not found: dave

import cats.Eval
val now = Eval.now(math.random + 1000)
// now: cats.Eval[Double] = Now(1000.6093560712978)
val later = Eval.later(math.random + 2000)
// later: cats.Eval[Double] = cats.Later@b38e255
val always = Eval.always(math.random + 3000)
// always: cats.Eval[Double] = cats.Always@7aa03230

now.value
later.value
always.value

val x = Eval.now {
  println("Computing X")
  math.random
}
// Computing X
// x: cats.Eval[Double] = Now(0.8561858941490939)
x.value // first access
// res9: Double = 0.8561858941490939
x.value // second access
// res10: Double = 0.8561858941490939

val y = Eval.always {
  println("Computing Y")
  math.random
}
// y: cats.Eval[Double] = cats.Always@3d65aec1
y.value // first access
// Computing Y
// res11: Double = 0.20044347463534973
y.value // second access
// Computing Y
// res12: Double = 0.6306326024648614

val z = Eval.later {
  println("Computing Z")
  math.random
}
// z: cats.Eval[Double] = cats.Later@6059069c
z.value // first access
// Computing Z
// res13: Double = 0.11754104909945928
z.value // second access
// res14: Double = 0.11754104909945928

val greeting = Eval.always { println("Step 1"); "Hello" }.map { str =>
  println("Step 2"); s"$str world"
}
// greeting: cats.Eval[String] = cats.Eval$$anon$8@497e6146
greeting.value
// Step 1
// Step 2
// res15: String = Hello world

val ans = for {
  a <- Eval.now { println("Calculating A"); 40 }
  b <- Eval.always { println("Calculating B"); 2 }
} yield {
  println("Adding A and B")
  a + b
}
// Calculating A
// ans: cats.Eval[Int] = cats.Eval$$anon$8@6ac067b7
ans.value // first access
// Calculating B
// Adding A and B
// res16: Int = 42
ans.value // second access
// Calculating B
// Adding A and B
// res17: Int = 42
val saying = Eval
  .always { println("Step 1"); "The cat" }
  .map { str =>
    println("Step 2"); s"$str sat on"
  }
  .memoize
  .map { str =>
    println("Step 3"); s"$str the mat"
  }
// saying: cats.Eval[String] = cats.Eval$$anon$8@225eed8c
saying.value // first access
// Step 1
// Step 2
// Step 3
// res18: String = The cat sat on the mat
saying.value // second access
// Step 3
// res19: String = The cat sat on the mat

def factorial(n: BigInt): BigInt =
  if (n == 1) n else n * factorial(n - 1)
//factorial(50000)
// java.lang.StackOverflowError

def factorial2(n: BigInt): Eval[BigInt] =
  if (n == 1) {
    Eval.now(n)
  } else {
    factorial2(n - 1).map(_ * n)
  }
//factorial2(50000).value
// java.lang.StackOverflowError

def factorial3(n: BigInt): Eval[BigInt] = {
  if (n == 1) {
    Eval.now(n)
  } else {
    Eval.defer(factorial3(n - 1).map(_ * n))
  }
}

factorial3(50000).value

def foldRightEval[A, B](as: List[A],
                        acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
  as match {
    case head :: tail =>
      Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
    case Nil =>
      acc
  }
def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
  foldRightEval(as, Eval.now(acc)) { (a, b) =>
    b.map(fn(a, _))
  }.value

foldRight((1 to 100000).toList, 0L)(_ + _)
