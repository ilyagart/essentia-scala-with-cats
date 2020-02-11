package sandbox

import cats.Monad
import cats.data.Reader
import cats.instances.string._
import cats.syntax.semigroup._
import cats.instances.option._
import cats.instances.list._
import cats.syntax.applicative._

object Main extends App {
  println("Hello " |+| "Cats!")
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

  println(sumSquare(Option(3), Option(4)))
  // res8: Option[Int] = Some(25)
  println(sumSquare(List(1, 2, 3), List(4, 5)))
  // res9: List[Int] = List(17, 26, 20, 29, 25, 34)
  println(sumSquare2(List(1, 2, 3), List(4, 5)))
  case class Db(usernames: Map[Int, String], passwords: Map[String, String])

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))
  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      username <- findUsername(userId)
      passwordOk <- username
        .map { username =>
          checkPassword(username, password)
        }
        .getOrElse { false.pure[DbReader] }
    } yield passwordOk

  val users = Map(1 -> "dade", 2 -> "kate", 3 -> "margo")
  val passwords =
    Map("dade" -> "zerocool", "kate" -> "acidburn", "margo" -> "secret")
  val db = Db(users, passwords)
  println(checkLogin(1, "zerocool").run(db))
  // res10: cats.Id[Boolean] = true
  println(checkLogin(4, "davinci").run(db))
  // res11: cats.Id[Boolean] = false
}
