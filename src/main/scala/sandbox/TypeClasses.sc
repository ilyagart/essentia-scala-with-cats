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
    w.write(value)
}

import JsonWriterInstances._
Json.toJson(Person("John", "user@example.com"))(personWriter)
Json.toJson(Person("John", "user@example.com"))

object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }
}
import JsonSyntax._
Person("John", "user@example.com").toJson

