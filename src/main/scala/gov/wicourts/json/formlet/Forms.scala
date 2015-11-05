package gov.wicourts.json.formlet

import argonaut._
import argonaut.Argonaut._

import scalaz.\/
import scalaz.OptionT.optionT
import scalaz.std.option._
import scalaz.syntax.applicative._
import scalaz.syntax.bind._
import scalaz.syntax.either._
import scalaz.syntax.std.option._

import scalaz.Id.Id

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
      val result = optionT[List[String] \/ ?](
        c.field(name).filterNot(_.isNull).right[List[String]]
      ).flatMapF(j =>
        j.right[List[String]].ensure(
          List(s"Field $name must be a $descr")
        )(
          matches
        ) >>= (fromJson(_).toRightDisjunction(
          List("internal error: expected a $descr")
        ))
      ).run.validation

      val view = FieldView(name, (result.toOption.join orElse value).map(toJson), None)

      (result, view).point[Id]
    })

  def row[A](field: FieldFormlet[A]): ObjectFormlet[A] =
    field.mapResult((a, v) => (
      a.leftMap(l => JsonObjectBuilder.row(v.name, Json.array(l.map(jString(_)): _*))),
      v.toJsonObjectBuilder
    ))

  def string(name: String, value: Option[String]): FieldFormlet[Option[String]] =
    primitive("string", jString(_), _.isString, _.string.map(_.trim), name, value)

  def number(name: String, value: Option[JsonNumber]): FieldFormlet[Option[JsonNumber]] =
    primitive("number", jNumber(_), _.isNumber, _.number, name, value)

  def boolean(name: String, value: Option[Boolean]): FieldFormlet[Option[Boolean]] =
    primitive("boolean", jBool(_), _.isBool, _.bool, name, value)

  def label[A](field: FieldFormlet[A], label: String): FieldFormlet[A] =
    field.mapView(FieldView.label.set(_, label.some))

  def required[A](field: FieldFormlet[Option[A]]): FieldFormlet[A] =
    field.mapValidation(_.toSuccess(List("This field is required")))
}
