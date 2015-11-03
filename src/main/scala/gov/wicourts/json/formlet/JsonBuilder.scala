package gov.wicourts.json.formlet

import argonaut.Json

import scalaz.Monoid

sealed trait JsonBuilder {
  def toJson: Json
}

class JsonArrayBuilder private [formlet] (private val items: List[Json]) extends JsonBuilder {
  def toJson: Json = Json.array(items: _*)
}

object JsonArrayBuilder {
  implicit val monoid: Monoid[JsonArrayBuilder] = new Monoid[JsonArrayBuilder] {
    def zero: JsonArrayBuilder = new JsonArrayBuilder(Nil)

    def append(f1: JsonArrayBuilder, f2: => JsonArrayBuilder): JsonArrayBuilder =
      new JsonArrayBuilder(f1.items ++ f2.items)
  }
}

class JsonObjectBuilder private [formlet] (
  private val items: List[(String, Json)]
) extends JsonBuilder {
  def toJson: Json = Json.obj(items: _*)

  override def toString: String = "JsonObjectBuilder(" + items.mkString(", ") + ")"
}

object JsonObjectBuilder {
  implicit val monoid: Monoid[JsonObjectBuilder] = new Monoid[JsonObjectBuilder] {
    def zero: JsonObjectBuilder = new JsonObjectBuilder(Nil)

    def append(f1: JsonObjectBuilder, f2: => JsonObjectBuilder): JsonObjectBuilder =
      new JsonObjectBuilder(f1.items ++ f2.items)
  }
}
