// Define a very simple JSON AST
sealed trait Json

final case class JsObject(get: Map[String, Json]) extends Json

final case class JsString(get: String) extends Json

final case class JsNumber(get: Double) extends Json

case object JsNull extends Json

// The "serialize to JSON" behaviour is encoded in this trait
trait JsonWriter[A] {
  def write(value: A): Json
}

final case class Person(name: String, email: String)

object JsonWriterInstances {
  implicit val stringWriter: JsonWriter[String] = (value: String) =>
    JsString(value)
  implicit val personWriter: JsonWriter[Person] = (value: Person) =>
    JsObject(
      Map("name" -> JsString(value.name), "email" -> JsString(value.email))
  )
}

object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
    w write value
}

import JsonWriterInstances._
Json.toJson(Person("John", "user@example.com"))
Json.toJson(Person("John", "user@example.com"))(personWriter)

object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json =
      w write value
  }
}
import JsonSyntax._
Person("John", "user@example.com").toJson
Person("Dave", "dave@example.com").toJson(personWriter)

implicitly[JsonWriter[String]]

Json.toJson("A string!")

implicit def optionWriter[A](
  implicit writer: JsonWriter[A]
): JsonWriter[Option[A]] = {
  case Some(aValue) => writer.write(aValue)
  case None         => JsNull
}

Json.toJson(Option("A string"))

trait Printable[A] {
  def format(value: A): String
}

object PrintableInstances {
  implicit val stringPrinter: Printable[String] = (value: String) => value
  implicit val intPrinter: Printable[Int] = (value: Int) => value.toString
}

object Printable {
  def format[A](input: A)(implicit p: Printable[A]): String = p.format(input)
  def print[A](input: A)(implicit p: Printable[A]): Unit =
    println(format(input))
}

final case class Cat(name: String, age: Int, color: String)

val cat = Cat("Vasilii", 5, "White")
import PrintableInstances._
implicit val catPrinter: Printable[Cat] = (cat: Cat) => {
  val name = Printable.format(cat.name)
  val age = Printable.format(cat.age)
  val color = Printable.format(cat.color)
  s"$name is a $age year-old $color cat."
}
//NAME is a AGE year-old COLOR cat.
Printable.print(cat)

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String = p.format(value)
    def print(implicit p: Printable[A]): Unit = println(format(p))
    def print2(implicit p: Printable[A]): Unit = println(format)
  }
}

import PrintableSyntax._
cat.print
cat.print2