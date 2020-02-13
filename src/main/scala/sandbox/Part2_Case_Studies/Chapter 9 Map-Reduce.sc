import cats.Monoid
import cats.syntax.semigroup._
import cats.implicits._

import scala.concurrent.Await

def foldMap[A, B: Monoid](xs: Vector[A])(func: A => B): B =
  xs.foldLeft(Monoid[B].empty)(_ |+| func(_)) //could .map(func).foldMap... instead

foldMap(Vector(1, 2, 3))(identity)
// res2: Int = 6
// Mapping to a String uses the concatenation monoid:
foldMap(Vector(1, 2, 3))(_.toString + "! ")
// res4: String = "1! 2! 3! "
// Mapping over a String to produce a String:
foldMap("Hello world!".toVector)(_.toString.toUpperCase)
// res6: String = HELLO WORLD!

/*
1. we start with an ini􀦞al list of all the data we need to process;
2. we divide the data into batches, sending one batch to each CPU;
3. the CPUs run a batch-level map phase in parallel;
4. the CPUs run a batch-level reduce phase in parallel, producing a local
result for each batch;
5. we reduce the results for each batch to a single final result.
   */

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

val future1 = Future {
  (1 to 100).toList.foldLeft(0)(_ + _)
}
// future1: scala.concurrent.Future[Int] = Future(<not completed>)
val future2 = Future {
  (100 to 200).toList.foldLeft(0)(_ + _)
}
// future2: scala.concurrent.Future[Int] = Future(<not completed>)
val future3 = future1.map(_.toString)
// future3: scala.concurrent.Future[String] = Future(<not completed>)
val future4 = for {
  a <- future1
  b <- future2
} yield a + b
// future4: scala.concurrent.Future[Int] = Future(<not completed>)

Runtime.getRuntime.availableProcessors
// res15: Int = 8
(1 to 10).toList.grouped(3).toList
// res16: List[List[Int]] = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9), List(10))

def parallelFoldMap[A, B: Monoid](
  values: Vector[A]
)(func: A => B): Future[B] = {
  val cpus = Runtime.getRuntime.availableProcessors
  val groupSize = (1.0 * values.size / cpus).ceil.toInt

  val groups: Iterator[Vector[A]] = values.grouped(groupSize)

  val futures: Iterator[Future[B]] =
    groups map { group =>
      Future(foldMap(group)(func))
    }
  Future.sequence(futures).map { iterable =>
    iterable.foldLeft(Monoid[B].empty)(_ |+| _)
  }
}

val result: Future[Int] = parallelFoldMap((1 to 1000000).toVector)(identity)
import scala.concurrent.duration._
Await.result(result, 1.second)

import cats.{Traverse, Foldable}
import cats.instances.int._ // for Monoid
import cats.instances.future._ // for Applicative and Monad
import cats.instances.vector._ // for Foldable and Traverse
import cats.syntax.semigroup._ // for |+|
import cats.syntax.foldable._ // for combineAll and foldMap
import cats.syntax.traverse._ // for traverse
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

def parallelFoldMap2[A, B: Monoid](
  values: Vector[A]
)(func: A => B): Future[B] = {
  val cpus = Runtime.getRuntime.availableProcessors
  val groupSize = (1.0 * values.size / cpus).ceil.toInt
  values
    .grouped(groupSize)
    .toVector
    .traverse(group => Future(group.foldMap(func)))
    .map(_.combineAll)
}

val future: Future[Int] =
  parallelFoldMap2((1 to 1000).toVector)(_ * 1000)
Await.result(future, 1.second)