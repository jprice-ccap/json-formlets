package gov.wicourts.json.formlet

import argonaut._
import argonaut.Argonaut._

import scalaz.\/
import scalaz.OptionT.optionT
import scalaz.std.option._
import scalaz.syntax.bind._
import scalaz.syntax.std.option._
import scalaz.syntax.either._

object Forms {
  private def primitive[A](
    descr: String,
    toJson: A => Json,
    matches: Json => Boolean,
    fromJson: Json => Option[A],
    name: String,
    value: Option[A]
  ): FieldFormlet[Option[A]] =
    Formlet(c => {
      val result = optionT[JsonObjectBuilder \/ ?](
        c.field(name).filterNot(_.isNull).right[JsonObjectBuilder]
      ).flatMapF(j =>
        j.right[JsonObjectBuilder].ensure(
          JsonObjectBuilder.row(name, jString(s"Field $name must be a $descr"))
        )(
          matches
        ) >>= (fromJson(_).toRightDisjunction(
          JsonObjectBuilder.row(name, jString("internal error: expected a $descr"))
        ))
      ).run.validation

      val view = FieldView(name, (result.toOption.join orElse value).map(toJson), None)

      (result, view)
    })

  def string(name: String, value: Option[String]): FieldFormlet[Option[String]] =
    primitive("string", jString(_), _.isString, _.string.map(_.trim), name, value)

  def number(name: String, value: Option[JsonNumber]): FieldFormlet[Option[JsonNumber]] =
    primitive("number", jNumber(_), _.isNumber, _.number, name, value)

  def boolean(name: String, value: Option[Boolean]): FieldFormlet[Option[Boolean]] =
    primitive("boolean", jBool(_), _.isBool, _.bool, name, value)

  def row[A](field: FieldFormlet[A]): ObjectFormlet[A] =
    field.mapView(_.toJsonObjectBuilder)

  def label[A](field: FieldFormlet[A], label: String): FieldFormlet[A] =
    field.mapView(FieldView.label.set(_, label.some))
}
