import cats.Applicative
import cats.data.Writer
import cats.instances.vector._

import scala.concurrent.{Await, Future} // for Monoid

Writer(Vector("It was the best of times", "it was the worst of times"), 1859)
// res0: cats.data.WriterT[cats.Id,scala.collection.immutable.Vector[String],Int] =
// WriterT((Vector(It was the best of times, it was the worst of times),1859))

import cats.syntax.applicative._ // for pure
type Logged[A] = Writer[Vector[String], A]
123.pure[Logged]
// res2: Logged[Int] = WriterT((Vector(),123))

import cats.syntax.writer._ // for tell
Vector("msg1", "msg2", "msg3").tell
// res3: cats.data.Writer[scala.collection.immutable.Vector[String],Unit] =
// WriterT((Vector(msg1, msg2, msg3),()))
val a = Writer(Vector("msg1", "msg2", "msg3"), 123)
// a: cats.data.WriterT[cats.Id,scala.collection.immutable.Vector[String],Int] =
// WriterT((Vector(msg1, msg2, msg3),123))
val b = 123.writer(Vector("msg1", "msg2", "msg3"))
// b: cats.data.Writer[scala.collection.immutable.Vector[String],Int]=
// WriterT((Vector(msg1, msg2, msg3),123))
val aResult: Int =
  a.value
// aResult: Int = 123
val aLog: Vector[String] =
  a.written
// aLog: Vector[String] = Vector(msg1, msg2, msg3)
val (log, result) = b.run
// log: scala.collection.immutable.Vector[String] = Vector(msg1, msg2,msg3)
// result: Int = 123
val writer1 = for {
  a <- 10.pure[Logged]
  _ <- Vector("a", "b", "c").tell
  b <- 32.writer(Vector("x", "y", "z"))
} yield a + b
// writer1: cats.data.WriterT[cats.Id,Vector[String],Int] =
// WriterT((Vector(a, b, c, x, y, z),42))
writer1.run
// res4: cats.Id[(Vector[String], Int)] =
// (Vector(a, b, c, x, y, z),42)
val writer2 = writer1.mapWritten(_.map(_.toUpperCase))
// writer2: cats.data.WriterT[cats.Id,scala.collection.immutable.
// Vector[String],Int] = WriterT((Vector(A, B, C, X, Y, Z),42))
writer2.run
// res5: cats.Id[(scala.collection.immutable.Vector[String], Int)] = (
// Vector(A, B, C, X, Y, Z),42)
val writer3 = writer1.bimap(log => log.map(_.toUpperCase), res => res * 100)
// writer3: cats.data.WriterT[cats.Id,scala.collection.immutable.
// Vector[String],Int] = WriterT((Vector(A, B, C, X, Y, Z),4200))
writer3.run
// res6: cats.Id[(scala.collection.immutable.Vector[String], Int)] = (
// Vector(A, B, C, X, Y, Z),4200)
val writer4 = writer1.mapBoth { (log, res) =>
  val log2 = log.map(_ + "!")
  val res2 = res * 1000
  (log2, res2)
}
// writer4: cats.data.WriterT[cats.Id,scala.collection.immutable.
// Vector[String],Int] = WriterT((Vector(a!, b!, c!, x!, y!, z!),42000))
writer4.run
// res7: cats.Id[(scala.collection.immutable.Vector[String], Int)] = (
// Vector(a!, b!, c!, x!, y!, z!),42000)
val writer5 = writer1.reset
// writer5: cats.data.WriterT[cats.Id,Vector[String],Int] = WriterT((
// Vector(),42))
writer5.run
// res8: cats.Id[(Vector[String], Int)] = (Vector(),42)
val writer6 = writer1.swap
// writer6: cats.data.WriterT[cats.Id,Int,Vector[String]] = WriterT
// ((42,Vector(a, b, c, x, y, z)))
writer6.run
// res9: cats.Id[(Int, Vector[String])] = (42,Vector(a, b, c, x, y, z))
def slowly[A](body: => A) =
  try body
  finally Thread.sleep(100)
def factorial(n: Int): Int = {
  val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
  println(s"fact $n $ans")
  ans
}
factorial(5)

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
//Await.result(
//  Future.sequence(Vector(Future(factorial(3)), Future(factorial(3)))),
//  5.seconds
//)

import cats.syntax.applicative._ // for pure
42.pure[Logged]
// res13: Logged[Int] = WriterT((Vector(),42))
// We’ll import the tell syntax as well:
import cats.syntax.writer._ // for tell
Vector("Message").tell
// res14: cats.data.Writer[scala.collection.immutable.Vector[String],Unit] =
// WriterT((Vector(Message),()))
41.pure[Logged].map(_ + 1)
// res15: cats.data.WriterT[cats.Id,Vector[String],Int] = WriterT((
// Vector(),42))

def factorial2(n: Int): Logged[Int] =
  for {
    ans <- if (n == 0) {
      1.pure[Logged]
    } else {
      slowly(factorial2(n - 1).map(_ * n))
    }
    _ <- Vector(s"fact $n $ans").tell
  } yield ans

factorial2(32)
val (log, res) = factorial2(5).run
// log: Vector[String] = Vector(fact 0 1, fact 1 1, fact 2 2, fact 3 6, fact 4 24, fact 5 120)
// res: Int = 120
//val Vector((logA, ansA), (logB, ansB)) =
//  Await.result(
//    Future.sequence(
//      Vector(Future(factorial2(3).run), Future(factorial2(5).run))
//    ),
//    5.seconds
//  )
// logA: Vector[String] = Vector(fact 0 1, fact 1 1, fact 2 2, fact 36)
// ansA: Int = 6
// logB: Vector[String] = Vector(fact 0 1, fact 1 1, fact 2 2, fact 36, fact 4 24, fact 5 120)
// ansB: Int = 120

import cats.data.Reader

case class Cat(name: String, favoriteFood: String)
// defined class Cat
val catName: Reader[Cat, String] =
  Reader(cat => cat.name)
// catName: cats.data.Reader[Cat,String] = Kleisli(<function1>)
catName.run(Cat("Garfield", "lasagne"))
// res0: cats.Id[String] = Garfield
val greetKitty: Reader[Cat, String] =
  catName.map(name => s"Hello $name")
greetKitty.run(Cat("Heathcliff", "junk food"))
// res1: cats.Id[String] = Hello Heathcliff
val feedKitty: Reader[Cat, String] =
  Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")
val greetAndFeed: Reader[Cat, String] =
  for {
    greet <- greetKitty
    feed <- feedKitty
  } yield s"$greet. $feed."
greetAndFeed(Cat("Garfield", "lasagne"))
// res3: cats.Id[String] = Hello Garfield. Have a nice bowl of lasagne.
greetAndFeed(Cat("Heathcliff", "junk food"))
// res4: cats.Id[String] = Hello Heathcliff. Have a nice bowl of junk food.

case class Db(usernames: Map[Int, String], passwords: Map[String, String])

type DbReader[A] = Reader[Db, A]

def findUsername(userId: Int): DbReader[Option[String]] =
  Reader(db => db.usernames.get(userId))
def checkPassword(username: String, password: String): DbReader[Boolean] =
  Reader(db => db.passwords.get(username).contains(password))

def checkLogin(userId: Int, password: String): DbReader[Boolean] =
  for {
    username <- findUsername(userId)
    passwordOk <- username.map { username =>
      checkPassword(username, password)
    }.get
//      .getOrElse { false.pure[DbReader] }
  } yield passwordOk

val users = Map(1 -> "dade", 2 -> "kate", 3 -> "margo")
val passwords =
  Map("dade" -> "zerocool", "kate" -> "acidburn", "margo" -> "secret")
val db = Db(users, passwords)
checkLogin(1, "zerocool").run(db)
// res10: cats.Id[Boolean] = true
//checkLogin(4, "davinci").run(db)
// res11: cats.Id[Boolean] = false