package gov.wicourts.json.formlet

import argonaut.Argonaut.jString
import argonaut.Json

import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.monadPlus._

import scalaz.{@>, Lens}

import Predef.ArrowAssoc

case class FieldView(name: String, value: Option[Json], label: Option[String], errorName: String) {
  def obj: JsonObjectBuilder = {
    val metadataItems =
      List(
        label.map(l => "label" -> jString(l))
      ).unite

    val metadata = metadataItems.headOption.as(
      ("metadata", Json.obj(metadataItems: _*))
    ).toList

    val valueItem = value.map(("value", _)).toList

    val all = valueItem ++ metadata

    new JsonObjectBuilder(
      if (all.isEmpty) Nil
      else List(name -> Json.obj(all: _*))
    )
  }

  def toJson: Json = obj.toJson
}

object FieldView {
  val label: FieldView @> Option[String] =
    Lens.lensu[FieldView, Option[String]](
      (a, value) => a.copy(label = value),
      _.label
    )

  val errorName: FieldView @> String =
    Lens.lensu[FieldView, String](
      (a, value) => a.copy(errorName = value),
      _.errorName
    )
}
