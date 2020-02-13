import cats.Show
import cats.instances.int._
import cats.instances.string._
import cats.kernel.Eq // for Show
val showInt: Show[Int] = Show.apply[Int]
val showString: Show[String] = Show.apply[String]

val intAsString: String = showInt.show(123)
// intAsString: String = 123
val stringAsString: String = showString.show("abc")
// stringAsString: String = abc

import cats.syntax.show._
val shownInt = 123.show
// shownInt: String = 123
val shownString = "abc".show
// shownString: String = abc

import java.util.Date
implicit val dateShow: Show[Date] =
  Show.show(date => s"${date.getTime}ms since the epoch.")
//(date: Date) => s"${date.getTime}ms since the epoch."
import java.awt.Point
implicit val pointShow: Show[Point] =
  Show.show(point => s"[x = ${point.x}, y = ${point.y}]")
//  (point: Point) => s"[x = ${point.x}, y = ${point.y}]"

val date = new Date()
date.show
s"${date.getTime / 1000 / 60 / 60 / 24 / 365.25} years since 1970"
val point = new Point(25, 36)
point.show

final case class Cat(name: String, age: Int, color: String)
implicit val catShow: Show[Cat] = { cat =>
  val name = cat.name
  val age = cat.age
  val color = cat.color
  s"$name is a $age year-old $color cat."
}

val cat = Cat("Petru", 1, "Dark")
cat.show

List(1, 2, 3) map Option[Int] filter (item => item.contains(1)) map {
  case None    => List.empty
  case Some(v) => v
}

val eqInt = Eq[Int]
eqInt.eqv(123, 123)
eqInt.eqv(123, 234)
import cats.syntax.eq._
123 === 234
123 =!= 234
//123==="123"
// Error:(56, 7) type mismatch;
//found   : String("123")
//required: Int
//123==="123"

import cats.instances.option._
(Some(1): Option[Int]) === (None: Option[Int])

import java.util.Date
import cats.instances.long._ // for Eq
implicit val dateEq: Eq[Date] =
  Eq.instance[Date] { (date1, date2) =>
    date1.getTime === date2.getTime
  }
val x = new Date() // now
val y = new Date() // a bit later than now
x === x
// res13: Boolean = true
x === y
// res14: Boolean = false
implicit val catEqual: Eq[Cat] = Eq.instance[Cat] { (cat1, cat2) =>
  {
    cat1.name === cat2.name &&
    cat1.color === cat2.color &&
    cat1.age === cat2.age
  }
}

val cat1 = Cat("Garfield", 38, "orange and black")
val cat2 = Cat("Heathcliff", 33, "orange and black")
val optionCat1 = Option(cat1)
val optionCat2 = Option.empty[Cat]

cat1 === cat2
cat =!= cat2
optionCat1 === optionCat2
optionCat1 =!= optionCat2
