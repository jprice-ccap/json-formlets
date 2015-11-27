package gov.wicourts.json.formlet

import argonaut.Argonaut.jString
import argonaut.Json

import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.monad._
import scalaz.syntax.std.option._

import scalaz.{@>, Lens}

case class FieldView(name: String, value: Option[Json], label: Option[String]) {
  def obj: JsonObjectBuilder = {
    val metadataItems =
      List(
        label.map(l => ("label", jString(l)))
      ).map(_.toList).join

    val metadata = metadataItems.headOption.as(
      ("metadata", Json.obj(metadataItems: _*))
    ).toList

    val valueItem = value.map(("value", _)).toList

    val all = valueItem ++ metadata

    all.headOption.as {
      val j = new JsonObjectBuilder(all).toJson
      new JsonObjectBuilder(List((name, j)))
    }.orZero
  }

  def toJson: Json = obj.toJson
}

object FieldView {
  val label: FieldView @> Option[String] =
    Lens.lensu[FieldView, Option[String]](
      (a, value) => a.copy(label = value),
      _.label
    )
}
