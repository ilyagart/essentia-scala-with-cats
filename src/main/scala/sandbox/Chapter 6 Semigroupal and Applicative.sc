import cats.syntax.either._ // for catchOnly
def parseInt(str: String): Either[String, Int] =
  Either
    .catchOnly[NumberFormatException](str.toInt)
    .leftMap(_ => s"Couldn't read $str")
for {
  a <- parseInt("a")
  b <- parseInt("b")
  c <- parseInt("c")
} yield (a + b + c)
// res1: scala.util.Either[String,Int] = Left(Couldn't read a)

import cats.Semigroupal
import cats.instances.option._ // for Semigroupal
Semigroupal[Option].product(Some(123), Some("abc"))
// res0: Option[(Int, String)] = Some((123,abc))
Semigroupal[Option].product(None, Some("abc"))
// res1: Option[(Nothing, String)] = None
Semigroupal[Option].product(Some(123), None)
// res2: Option[(Int, Nothing)] = None
Semigroupal.tuple3(Option(1), Option(2), Option(3))
// res3: Option[(Int, Int, Int)] = Some((1,2,3))
Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int])
// res4: Option[(Int, Int, Int)] = None
Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)
// res5: Option[Int] = Some(6)
Semigroupal.map2(Option(1), Option.empty[Int])(_ + _)
// res6: Option[Int] = None
import cats.syntax.apply._ // for tupled and mapN
(Option(123), Option("abc")).tupled
// res7: Option[(Int, String)] = Some((123,abc))
(Option(123), Option("abc"), Option(true)).tupled

// res8: Option[(Int, String, Boolean)] = Some((123,abc,true))
case class CatG(name: String, born: Int, color: String)

(Option("Garfield"), Option(1978), Option("Orange & black"))
  .mapN(CatG.apply)
// res9: Option[Cat] = Some(Cat(Garfield,1978,Orange & black))

val add: (Int, Int) => Int = (a, b) => a + b
// add: (Int, Int) => Int = <function2>
//(Option(1), Option(2), Option(3)).mapN(add)
// <console>:27: error: type mismatch;
// found : (Int, Int) => Int
// required: (Int, Int, Int) => ?
// (Option(1), Option(2), Option(3)).mapN(add)
// ^
//(Option("cats"), Option(true)).mapN(add)
// <console>:27: error: type mismatch;
// found : (Int, Int) => Int
// required: (String, Boolean) => ?
// (Option("cats"), Option(true)).mapN(add)
//
import cats.Monoid
import cats.instances.int._
import cats.instances.invariant._
import cats.instances.list._
import cats.instances.string._
import cats.syntax.apply._ // for imapN
case class Cat(name: String, yearOfBirth: Int, favoriteFoods: List[String])
val tupleToCat: (String, Int, List[String]) => Cat = Cat.apply

val catToTuple: Cat => (String, Int, List[String]) = cat =>
  (cat.name, cat.yearOfBirth, cat.favoriteFoods)

implicit val catMonoid: Monoid[Cat] =
  (Monoid[String], Monoid[Int], Monoid[List[String]])
    .imapN(tupleToCat)(catToTuple)

import cats.syntax.semigroup._ // for |+|
val garfield = Cat("Garfield", 1978, List("Lasagne"))
val heathcliff = Cat("Heathcliff", 1988, List("Junk Food"))
garfield |+| heathcliff
// res17: Cat = Cat(GarfieldHeathcliff,3966,List(Lasagne, Junk Food))
import cats.Semigroupal
import cats.instances.future._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.higherKinds

val futurePair = Semigroupal[Future].product(Future("Hello"), Future(123))
Await.result(futurePair, 1.second)
// res1: (String, Int) = (Hello,123)
import cats.syntax.apply._ // for mapN
case class Cat3(name: String, yearOfBirth: Int, favoriteFoods: List[String])
val futureCat3 =
  (Future("Garfield"), Future(1978), Future(List("Lasagne"))).mapN(Cat3.apply)
Await.result(futureCat3, 1.second)
// res4: Cat = Cat(Garfield,1978,List(Lasagne))
import cats.instances.list._ // for Semigroupal
Semigroupal[List].product(List(1, 2), List(3, 4))
// res5: List[(Int, Int)] = List((1,3), (1,4), (2,3), (2,4))
import cats.implicits._

type ErrorOr[A] = Either[Vector[String], A]
Semigroupal[ErrorOr].product(Left(Vector("Error 1")), Left(Vector("Error 2")))
// res7: ErrorOr[(Nothing, Nothing)] = Left(Vector(Error 1))
import cats.Monad

def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
  for {
    a <- x
    b <- y
  } yield (a, b)
// should be equivalent to
def product2[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
  x.flatMap(a => y.map(b => (a, b)))

product(List(1, 2), List(3, 4))
// res12: List[(Int, Int)] = List((1,3), (1,4), (2,3), (2,4))
product2(List(1, 3), List(2, 4))
import cats.data.Validated

type AllErrorsOr[A] = Validated[List[String], A]

Semigroupal[AllErrorsOr].product(
  Validated.invalid(List("Error1")),
  Validated.invalid(List("Error2"))
)

val v0 = Validated.Valid(123)
// v: cats.data.Validated.Valid[Int] = Valid(123)
val i0 = Validated.Invalid(List("Badness"))
// i: cats.data.Validated.Invalid[List[String]] = Invalid(List(Badness))

val v = Validated.valid[List[String], Int](123)
// v: cats.data.Validated[List[String],Int] = Valid(123)
val i = Validated.invalid[List[String], Int](List("Badness"))
// i: cats.data.Validated[List[String],Int] = Invalid(List(Badness))

import cats.syntax.validated._ // for valid and invalid
123.valid[List[String]]
// res2: cats.data.Validated[List[String],Int] = Valid(123)
List("Badness").invalid[Int]
// res3: cats.data.Validated[List[String],Int] = Invalid(List(Badness))

import cats.syntax.applicative._ // for pure
import cats.syntax.applicativeError._ // for raiseError
type ErrorsOr[A] = Validated[List[String], A]
123.pure[ErrorsOr]
// res5: ErrorsOr[Int] = Valid(123)
List("Badness").raiseError[ErrorsOr, Int]
// res6: ErrorsOr[Int] = Invalid(List(Badness))
Validated.catchOnly[NumberFormatException]("foo".toInt)
// res7: cats.data.Validated[NumberFormatException,Int] = Invalid(java.lang.NumberFormatException: For input string: "foo")
Validated.catchNonFatal(sys.error("Badness"))
// res8: cats.data.Validated[Throwable,Nothing] = Invalid(java.lang.RuntimeException: Badness)
Validated.fromTry(scala.util.Try("foo".toInt))
// res9: cats.data.Validated[Throwable,Int] = Invalid(java.lang.NumberFormatException: For input string: "foo")
Validated.fromEither[String, Int](Left("Badness"))
// res10: cats.data.Validated[String,Int] = Invalid(Badness)
Validated.fromOption[String, Int](None, "Badness")
// res11: cats.data.Validated[String,Int] = Invalid(Badness)
import cats.syntax.apply._ // for tupled
import cats.instances.string._ // for Semigroup
type AllErrorsOr[A] = Validated[String, A]
//("Error 1".invalid[Int], "Error 2".invalid[Int]).tupled
// res14: cats.data.Validated[String,(Int, Int)] = Invalid(Error 1Error 2)
import cats.instances.vector._ // for Semigroupal
(Vector(404).invalid[Int], Vector(500).invalid[Int]).tupled
// res15: cats.data.Validated[scala.collection.immutable.Vector[Int],(Int, Int)] = Invalid(Vector(404, 500))
