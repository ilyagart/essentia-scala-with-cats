def show[A](list: List[A]): String =
  list.foldLeft("nil")((accum, item) => s"$item then $accum")
show(Nil)
// res0: String = nil
show(List(1, 2, 3))
// res1: String = 3 then 2 then 1 then nil

@scala.annotation.tailrec
def myFoldLeft[B](xs: List[B])(initialState: B)(op: (B, B) => B): B = {
  if (xs.isEmpty) initialState
  else myFoldLeft(xs.tail)(op(initialState, xs.head))(op)
}
@scala.annotation.tailrec
def myFoldRight[B](xs: List[B])(initialState: B)(op: (B, B) => B): B =
  if (xs.isEmpty) initialState
  else myFoldRight(xs.tail)(op(xs.head, initialState))(op)

myFoldLeft(List(1, 2, 3))(0)(_ + _)
myFoldLeft(List(1, 2, 3))(1)(_ * _)
myFoldLeft(List(1, 2, 3))(0)(_ - _)
myFoldRight(List(1, 2, 3))(0)(_ - _)
List(1, 2, 3, 4).sum
List(1, 2, 3, 4).foldLeft(0)(_ - _)
List(1, 2, 3, 4).product

myFoldLeft(List("1", "2", "3", "4"))("kek")(
  (accum, item) => s"$item then $accum"
)
myFoldRight(List("1", "2", "3", "4"))("kek")(
  (accum, item) => s"$item then $accum"
)

List(1, 2, 3).foldLeft(List.empty[Int])((a, b) => b :: a)
List(1, 2, 3).foldRight(List.empty[Int])((a, b) => a :: b)

def listMap[A, B](xs: List[A])(f: A => B): List[B] =
  xs.foldRight(List.empty[B]) { (item, accum) =>
    f(item) :: accum
  }

def listFlatMap[A, B](xs: List[A])(f: A => List[B]): List[B] =
  xs.foldRight(List.empty[B]) { (item, accum) =>
    f(item) ::: accum
  }

def listFilter[A](xs: List[A])(p: A => Boolean): List[A] =
  xs.foldRight(List.empty[A]) { (item, accum) =>
    {
      if (p(item)) item :: accum else accum
    }
  }

listMap(List(1, 2, 3))(_ + 1)
listFlatMap(List(1, 2, 3))(a => List(a, a * 10, a * 100))
listFilter(List(1, 2, 3))(_ * 2 < 3)

import cats.{Foldable, Traverse}
import cats.instances.list._
val ints2 = List(1, 2, 3)
Foldable[List].foldLeft(ints2, 0)(_ + _)
// res1: Int = 6
import cats.instances.option._ // for Foldable
val maybeInt = Option(123)
Foldable[Option].foldLeft(maybeInt, 10)(_ * _)
// res3: Int = 1230
import cats.{Eval, Foldable}
import cats.instances.stream._ // for Foldable
def bigData: Stream[Int] = (1 to 100000).toStream
val eval: Eval[Long] =
  Foldable[Stream].foldRight(bigData, Eval.now(0L)) { (num, eval) =>
    eval.map(_ + num)
  }
eval.value
// res7: Long = 5000050000
Foldable[Option].nonEmpty(Option(42))
// res10: Boolean = true
Foldable[List].find(List(1, 2, 3))(_ % 2 == 0)
// res11: Option[Int] = Some(2)
import cats.instances.int._
Foldable[List].combineAll(List(1, 2, 3))
Foldable[List].toList(List(3, 4, 5))
import cats.instances.string._ // for Monoid
Foldable[List].foldMap(List(1, 2, 3))(_.toString)
// res13: String = 123
import cats.instances.vector._ // for Monoid
val ints = List(Vector(1, 2, 3), Vector(4, 5, 6))
(Foldable[List] compose Foldable[Vector]).combineAll(ints)
// res15: Int = 21
import cats.syntax.foldable._ // for combineAll and foldMap
List(1, 2, 3).combineAll
// res16: Int = 6
List(1, 2, 3).foldMap(_.toString)
// res17: String = 123
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
val hostnames =
  List("alpha.example.com", "beta.example.com", "gamma.demo.com")
def getUptime(hostname: String): Future[Int] =
  Future(hostname.length * 60) // just for demonstration

val allUptimes: Future[List[Int]] =
  hostnames.foldLeft(Future(List.empty[Int])) { (accum, host) =>
    val uptime = getUptime(host)
    for {
      accum <- accum
      uptime <- uptime
    } yield accum :+ uptime
  }
Await.result(allUptimes, 1.second)
// res2: List[Int] = List(1020, 960, 840)
val allUptimes2: Future[List[Int]] =
  Future.traverse(hostnames)(getUptime)
Await.result(allUptimes2, 1.second)
// res3: List[Int] = List(1020, 960, 840)

import cats.instances.future._ // for Applicative
import cats.syntax.applicative._ // for pure
List.empty[Int].pure[Future]
def oldCombine(accum: Future[List[Int]], host: String): Future[List[Int]] = {
  val uptime = getUptime(host)
  for {
    accum <- accum
    uptime <- uptime
  } yield accum :+ uptime
}

import cats.syntax.apply._ // for mapN
// Combining accumulator and hostname using an Applicative:
def newCombine(accum: Future[List[Int]], host: String): Future[List[Int]] =
  (accum, getUptime(host)).mapN(_ :+ _)
import scala.language.higherKinds
import cats.Applicative
import cats.implicits._

def listTraverse[F[_]: Applicative, A, B](
  list: List[A]
)(func: A => F[B]): F[List[B]] =
  list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
    (accum, func(item)).mapN(_ :+ _)
  }
def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
  listTraverse(list)(identity)
listSequence(List(Vector(1, 2), Vector(3, 4)))
listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))
def process1(inputs: List[Int]): Option[List[Int]] =
  listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)
process1(List(2, 4, 6))
process1(List(1, 2, 3))
import cats.data.Validated
import cats.instances.list._ // for Monoid
type ErrorsOr[A] = Validated[List[String], A]
def process(inputs: List[Int]): ErrorsOr[List[Int]] =
  listTraverse(inputs) { n =>
    if (n % 2 == 0) {
      Validated.valid(n)
    } else {
      Validated.invalid(List(s"$n is not even"))
    }
  }
process(List(2, 4, 6))
process(List(1, 2, 3))
val numbers = List(Future(1), Future(2), Future(3))
val numbers2: Future[List[Int]] =
  Traverse[List].sequence(numbers)
Await.result(numbers2, 1.second)
// res3: List[Int] = List(1, 2, 3)
import cats.syntax.traverse._ // for sequence and traverse
Await.result(hostnames.traverse(getUptime), 1.second)
// res4: List[Int] = List(1020, 960, 840)
Await.result(numbers.sequence, 1.second)
// res5: List[Int] = List(1, 2, 3)