package gov.wicourts.json.formlet

import argonaut._
import argonaut.Argonaut._

import scalaz.\/
import scalaz.OptionT.optionT
import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.applicative._
import scalaz.syntax.bind._
import scalaz.syntax.either._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._

import scalaz.Id.Id

object Forms {
  private def primitive[A](
    descr: String,
    toJson: A => Json,
    matches: Json => Boolean,
    fromJson: Json => List[String] \/ A,
    name: String,
    value: Option[A]
  ): FieldFormlet[Option[A]] =
    Formlet(c => {
      val result = optionT[List[String] \/ ?](
        c.field(name).filterNot(_.isNull).right[List[String]]
      ).flatMapF(j =>
        j.right[List[String]].ensure(
          List(s"Field $name must be a(n) $descr")
        )(
          matches
        ) >>= (fromJson)
      ).run.validation

      val view = FieldView(name, (result.toOption.join orElse value).map(toJson), None)

      (result, view).point[Id]
    })

  def row[A](field: FieldFormlet[A]): ObjectFormlet[A] =
    field.mapResult((a, v) => (
      a.leftMap(l => JsonObjectBuilder.row(v.name, Json.array(l.map(jString(_)): _*))),
      v.toJsonObjectBuilder
    ))

  private def check[A](name: String, descr: String, a: Option[A]): List[String] \/ A =
    a.toRightDisjunction(List(s"Expected a $descr when processing field $name"))

  private def fromArray[A](
    name: String,
    descr: String,
    fromItem: Json => Option[A]
  ): Json => List[String] \/ List[A] = j =>
    check(name, s"array of $descr", j.array) >>= (_.traverseU(i => check(name, descr, fromItem(i))))

  def listOfString(name: String, value: Option[List[String]]): FieldFormlet[Option[List[String]]] =
    primitive(
      "array of string",
      l => Json.array(l.map(jString(_)): _*),
      _.isArray,
      fromArray(name, "string", _.string),
      name,
      value
    )

  def listOfNumber(name: String, value: Option[List[JsonNumber]]): FieldFormlet[Option[List[JsonNumber]]] =
    primitive(
      "array of number",
      l => Json.array(l.map(jNumber(_)): _*),
      _.isArray,
      fromArray(name, "number", _.number),
      name,
      value
    )

  def listOfBoolean(name: String, value: Option[List[Boolean]]): FieldFormlet[Option[List[Boolean]]] =
    primitive(
      "array of boolean",
      l => Json.array(l.map(jBool(_)): _*),
      _.isArray,
      fromArray(name, "boolean", _.bool),
      name,
      value
    )

  def string(name: String, value: Option[String]): FieldFormlet[Option[String]] =
    primitive(
      "string",
      jString(_),
      _.isString,
      ((_: Json).string.map(_.trim)) andThen (a => check(name, "string", a)),
      name,
      value
    )

  def number(name: String, value: Option[JsonNumber]): FieldFormlet[Option[JsonNumber]] =
    primitive(
      "number",
      jNumber(_),
      _.isNumber,
      ((_: Json).number) andThen (a => check(name, "number", a)),
      name,
      value
    )

  def boolean(name: String, value: Option[Boolean]): FieldFormlet[Option[Boolean]] =
    primitive(
      "boolean",
      jBool(_),
      _.isBool,
      ((_: Json).bool) andThen (a => check(name, "boolean", a)),

      name,
      value
    )

  def label[A](field: FieldFormlet[A], label: String): FieldFormlet[A] =
    field.mapView(FieldView.label.set(_, label.some))

  def required[A](field: FieldFormlet[Option[A]]): FieldFormlet[A] =
    field.mapValidation(_.toSuccess(List("This field is required")))
}
