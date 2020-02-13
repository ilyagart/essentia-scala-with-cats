import cats.data.OptionT

type ListOption[A] = OptionT[List, A]
import cats.instances.list._
import cats.syntax.applicative._
import cats.data.{EitherT, OptionT}
import cats.implicits._
import scala.concurrent.Future

import scala.concurrent.Await
import scala.concurrent.duration._
val result1: ListOption[Int] = OptionT(List(Option(10)))
// result1: ListOption[Int] = OptionT(List(Some(10)))
val result2: ListOption[Int] = 32.pure[ListOption]
// result2: ListOption[Int] = OptionT(List(Some(32)))
result1.flatMap { x: Int =>
  result2.map { y: Int =>
    x + y
  }
}
// res1: cats.data.OptionT[List,Int] = OptionT(List(Some(42)))
// Alias Either to a type constructor with one parameter:
type ErrorOr[A] = Either[String, A]
// Build our final monad stack using OptionT:
type ErrorOrOption[A] = OptionT[ErrorOr, A] // for Monad
val a = 10.pure[ErrorOrOption]
// a: ErrorOrOption[Int] = OptionT(Right(Some(10)))
val b = 32.pure[ErrorOrOption]
// b: ErrorOrOption[Int] = OptionT(Right(Some(32)))
val c = a.flatMap(x => b.map(y => x + y))
// c: cats.data.OptionT[ErrorOr,Int] = OptionT(Right(Some(42)))
for {
  x <- a
  y <- b
} yield x + y

type FutureEither[A] = EitherT[Future, String, A]
type FutureEitherOption[A] = OptionT[FutureEither, A]

val errorStack1 = OptionT[ErrorOr, Int](Right(Some(10)))
val errorStack2 = 32.pure[ErrorOrOption]

errorStack1.value
// res11: ErrorOr[Option[Int]] = Right(Some(10))
// Mapping over the Either in the stack:
errorStack2.value.map(_.getOrElse(-1))
// res13: scala.util.Either[String,Int] = Right(32)

for {
  a <- errorStack1
  b <- errorStack2
} yield a + b

import scala.concurrent.ExecutionContext.Implicits.global
val futureEitherOr: FutureEitherOption[Int] =
  for {
    a <- 10.pure[FutureEitherOption]
    b <- 32.pure[FutureEitherOption]
  } yield a + b

futureEitherOr
// res14: FutureEitherOption[Int] = OptionT(EitherT(Future(Success(
//Right(Some(42))))))
val intermediate = futureEitherOr.value
// intermediate: FutureEither[Option[Int]] = EitherT(Future(Success(
//Right(Some(42)))))
val stack = intermediate.value
// stack: scala.concurrent.Future[Either[String,Option[Int]]] = Future(Success(Right(Some(42))))
Await.result(stack, 1.second)
// res15: Either[String,Option[Int]] = Right(Some(42))

import cats.data.Writer

type Logged[A] = Writer[List[String], A]
// Methods generally return untransformed stacks:
def parseNumber(str: String): Logged[Option[Int]] =
  util.Try(str.toInt).toOption match {
    case Some(num) => Writer(List(s"Read $str"), Some(num))
    case None      => Writer(List(s"Failed on $str"), None)
  }
// Consumers use monad transformers locally to simplify composition:
def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
  import cats.data.OptionT
  val result = for {
    a <- OptionT(parseNumber(a))
    b <- OptionT(parseNumber(b))
    c <- OptionT(parseNumber(c))
  } yield a + b + c
  result.value
}
// This approach doesn't force OptionT on other users' code:
val result1 = addAll("1", "2", "3")
// result1: Logged[Option[Int]] = WriterT((List(Read 1, Read 2, Read3),Some(6)))
val result2 = addAll("1", "a", "3")
// result2: Logged[Option[Int]] = WriterT((List(Read 1, Failed on a),None))

type Response[A] = EitherT[Future, String, A]
// defined type alias Response

val powerLevels = Map("Jazz" -> 6, "Bumblebee" -> 8, "Hot Rod" -> 10)
def getPowerLevel(autobot: String): Response[Int] = {
  powerLevels.get(autobot) match {
    case Some(value) => EitherT.right(Future(value))
    case None =>
      EitherT.left(Future(s"Friendly autobot $autobot is unreachable :c"))
  }
}

getPowerLevel("Bumblebee").value
getPowerLevel("Hot Rod").value
getPowerLevel("Arthur").value

def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
  for {
    power1 <- getPowerLevel(ally1)
    power2 <- getPowerLevel(ally2)
  } yield (power1 + power2) > 15

def tacticalReport(ally1: String, ally2: String): String = {
  val stack = canSpecialMove(ally1, ally2).value
  Await.result(stack, 1.second) match {
    case Left(msg) => s"Comms error: $msg"
    case Right(true) =>
      s"Hurray,$ally1 and $ally2 are ready to do the special move!"
    case Right(false) => "Can't perform a special move, vi lost :C"
  }
}
tacticalReport("Jazz", "Bumblebee")
// res28: String = Jazz and Bumblebee need a recharge.
tacticalReport("Bumblebee", "Hot Rod")
// res29: String = Bumblebee and Hot Rod are ready to roll out!
tacticalReport("Jazz", "Ironhide")
// res30: String = Comms error: Ironhide unreachable
