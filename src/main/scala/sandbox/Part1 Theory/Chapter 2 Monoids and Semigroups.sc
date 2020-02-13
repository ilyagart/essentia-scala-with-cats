//trait Semigroup[A] {
//  def combine(x: A, y: A): A
//}
//
//trait Monoid[A] extends Semigroup[A] {
//  def empty: A
//}
//
//object Monoid {
//  def apply[A](implicit monoid: Monoid[A]) =
//    monoid
//}
//
//implicit val booleanAndMonoid: Monoid[Boolean] =
//  new Monoid[Boolean] {
//    def combine(x: Boolean, y: Boolean): Boolean = x && y
//    def empty = true
//  }
//
//implicit val booleanOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
//  def empty = false
//  def combine(x: Boolean, y: Boolean) = x || y
//}
//
//implicit val booleanEitherMonoid: Monoid[Boolean] = new Monoid[Boolean] {
//  def empty = false
//  def combine(x: Boolean, y: Boolean) = (x && !y) || (!x && y)
//}
//
//implicit val booleanXNorMonoid: Monoid[Boolean] = new Monoid[Boolean] {
//  def empty = true
//  def combine(x: Boolean, y: Boolean) = (!x || y) && (x || !y)
//}

import cats.Monoid
import cats.instances.string._ // for Monoid
Monoid[String].combine("Hi ", "there")
// res0: String = Hi there
Monoid[String].empty
// res1: String = ""
import cats.instances.int._
Monoid[Int].combine(1, 2345)

import cats.instances.option._ // for Monoid
val a = Option(22)
// a: Option[Int] = Some(22)
val b = Option(20)
// b: Option[Int] = Some(20)
Monoid[Option[Int]].combine(a, b)
// res6: Option[Int] = Some(42)

import cats.syntax.semigroup._ // for |+|
val stringResult = "Hi " |+| "there" |+| Monoid[String].empty
// stringResult: String = Hi there
import cats.instances.int._ // for Monoid
val intResult = 1 |+| 2 |+| Monoid[Int].empty

//def add(items: List[Int]): Int = items.sum
def simpleAdd(items: List[Int]): Int =
  items.foldLeft(Monoid[Int].empty)(_ |+| _)

simpleAdd(List(1, 2, 3, 4, 69))

//def add(items: List[Option[Int]]): Int =
//  items.map {
//    case Some(value) => value
//    case None        => 0
//  }.sum

def add[A: Monoid](items: List[A]): A =
  items.foldLeft(Monoid[A].empty)(_ |+| _)

def monoidAdd[A](items: List[A])(implicit monoid: Monoid[A]): A =
  items.foldLeft(Monoid[A].empty)(_ |+| _)

add(List(Some(1), Some(4), None, Some(3)))
monoidAdd(List(Some(1), Some(4), None, Some(3)))

case class Order(totalCost: Double, quantity: Double)

implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
  def empty = Order(0, 0)
  def combine(x: Order, y: Order) =
    Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
}

val ord1 = Order(123, 321)
val ord2 = Order(123, 456)
add(List(ord1, ord2))

"Scala" |+| " with " |+| "Cats" |+| Monoid[String].empty

import cats.instances.map._ // for Monoid
val map1 = Map("a" -> 1, "b" -> 2)
val map2 = Map("b" -> 3, "d" -> 4)
map1 |+| map2
// res3: Map[String,Int] = Map(b -> 5, d -> 4, a -> 1)
import cats.instances.tuple._ // for Monoid
val tuple1 = ("hello", 123)
val tuple2 = ("world", 321)
tuple1 |+| tuple2
// res6: (String, Int) = (helloworld,444)