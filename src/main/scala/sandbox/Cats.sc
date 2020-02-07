import cats.Show
import cats.instances.int._ // for Show
import cats.instances.string._ // for Show
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
  case None => List.empty
  case Some(v) => v
}