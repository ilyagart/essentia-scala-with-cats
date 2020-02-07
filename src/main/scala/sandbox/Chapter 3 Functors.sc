import cats.kernel.Monoid

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
val future: Future[String] =
  Future(123).map(n => n + 1).map(n => n * 2).map(n => n + "!")
Await.result(future, 1.second)
// res3: String = 248!

import scala.util.Random
val future1 = {
  // Initialize Random with a fixed seed:
  val r = new Random(0L)
  // nextInt has the side-effect of moving to
  // the next random number in the sequence:
  val x = Future(r.nextInt)
  for {
    a <- x
    b <- x
  } yield (a, b)
}
val future2 = {
  val r = new Random(0L)
  for {
    a <- Future(r.nextInt)
    b <- Future(r.nextInt)
  } yield (a, b)
}
val result1 = Await.result(future1, 1.second)
// result1: (Int, Int) = (-1155484576,-1155484576)
val result2 = Await.result(future2, 1.second)
// result2: (Int, Int) = (-1155484576,-723955400)

import cats.instances.function._ // for Functor
import cats.syntax.functor._ // for map
val func11: Int => Double = (x: Int) => x.toDouble
val func22: Double => Double = (y: Double) => y * 2
//(func1 map func2)(1) // composition using map
// res7: Double = 2.0
(func11 andThen func22)(1) // composition using andThen
// res8: Double = 2.0
func22(func11(1)) // composition written out by hand
// res9: Double = 2.0

import cats.Functor
import cats.instances.list._ // for Functor
import cats.instances.option._ // for Functor
val list1 = List(1, 2, 3)
// list1: List[Int] = List(1, 2, 3)
val list2 = Functor[List].map(list1)(_ * 2)
// list2: List[Int] = List(2, 4, 6)
val option1 = Option(123)
// option1: Option[Int] = Some(123)
val option2 = Functor[Option].map(option1)(_.toString)
// option2: Option[String] = Some(123)

val func = (x: Int) => x + 1
// func: Int => Int = <function1>
val liftedFunc = Functor[Option].lift(func)
// liftedFunc: Option[Int] => Option[Int] = cats.Functor$$Lambda$11698/1630828883@722bf240
liftedFunc(Option(1))
// res0: Option[Int] = Some(2)

import cats.instances.int._
import cats.syntax.semigroup._
val someVal = Functor[List].map(List(1, 2, 3))(func).map(func).map(func)
def add[A: Monoid](xs: List[A]): A = {
  xs.foldLeft(Monoid[A].empty)(_ |+| _)
}

def addElem[A: Monoid](x: Int): Int = {
  x |+| 1
}
implicit val intAdditionMonoid: Monoid[Int] = new Monoid[Int] {
  def empty: Int = 0
  def combine(x: Int, y: Int): Int = x + y
}

import cats.Functor
import cats.instances.function._
import cats.syntax.functor._ // for map
Functor[List].map(List(1, 2, 3))(addElem(_)(intAdditionMonoid)) map (addElem(_)(intAdditionMonoid))
val func1 = (a: Int) => a + 1
val func2 = (a: Int) => a * 2
val func3 = (a: Int) => a + "!"
//val func4 = func1.map(func2).map(func3)
//func4(123)
// res1: String = 248!
def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] =
  start.map(n => n + 1 * 2)

import cats.instances.option._ // for Functor
import cats.instances.list._ // for Functor
doMath(Option(20))
// res3: Option[Int] = Some(22)
doMath(List(1, 2, 3))
// res4: List[Int] = List(3, 4, 5)

final case class Box[A](value: A)
val box = Box[Int](123)
implicit val optionFunctor: Functor[Option] =
  new Functor[Option] {
    def map[A, B](value: Option[A])(func: A => B): Option[B] =
      value.map(func)
  }
implicit val boxFunctor: Functor[Box] = new Functor[Box] {
  def map[A, B](fa: Box[A])(f: A => B) = Box(f(fa.value))
}
box.map(value => value + 1)

implicit def futureFunctor: Functor[Future] =
  new Functor[Future] {
    def map[A, B](value: Future[A])(func: A => B): Future[B] =
      value.map(func)
  }

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)
  def leaf[A](value: A): Tree[A] =
    Leaf(value)
}

implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
  def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
    case Leaf(value)         => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }
}

Tree.leaf(100).map(_ * 2)
// res10: wrapper.Tree[Int] = Leaf(200)
Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2)
// res11: wrapper.Tree[Int] = Branch(Leaf(20),Leaf(40))

