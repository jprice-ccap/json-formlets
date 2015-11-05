package gov.wicourts.json.formlet

import argonaut._
import argonaut.Argonaut._

import scalaz._
import scalaz.OptionT.optionT
import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.applicative._
import scalaz.syntax.bind._
import scalaz.syntax.either._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._

import scalaz.Id.Id

import scala.language.higherKinds

object Forms {
  private def primitive[M[_], A](
    descr: String,
    toJson: A => Json,
    matches: Json => Boolean,
    fromJson: Json => List[String] \/ A,
    name: String,
    value: Option[A]
  )(
    implicit M: Applicative[M]
  ): FieldFormlet[M, Option[A]] =
    Formlet { c =>
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

      (result, view).point[M]
    }

  def namedContext[M[_], E, A, V](
    name: String,
    inner: JsonFormlet[M, E, A, V]
  ): JsonFormlet[M, E, A, V] =
    Formlet(c => inner.run(c.field(name).getOrElse(jNull)))

  def nested[M[_], E, A](
    name: String,
    inner: JsonFormlet[M, E, A, JsonObjectBuilder]
  )(
    implicit M: Functor[M]
  ): JsonFormlet[M, E, A, JsonObjectBuilder] =
    namedContext(name, inner).mapView(o => JsonObjectBuilder.row(name, o.toJson))

  def nestedList[M[_], E, A](
    name: String,
    inner: JsonFormlet[M, E, A, JsonObjectBuilder]
  ): JsonFormlet[M, E, List[A], JsonObjectBuilder] = ???

  def row[M[_], A](
    field: FieldFormlet[M, A]
  )(
    implicit M: Functor[M]
  ): ObjectFormlet[M, A] =
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

  def listOfStringM[M[_]](
    name: String,
    value: Option[List[String]]
  )(
    implicit M: Applicative[M]
  ): FieldFormlet[M, Option[List[String]]] =
    primitive(
      "array of string",
      l => Json.array(l.map(jString(_)): _*),
      _.isArray,
      fromArray(name, "string", _.string),
      name,
      value
    )

  def listOfNumberM[M[_]](
    name: String,
    value: Option[List[JsonNumber]]
  )(
    implicit M: Applicative[M]
  ): FieldFormlet[M, Option[List[JsonNumber]]] =
    primitive(
      "array of number",
      l => Json.array(l.map(jNumber(_)): _*),
      _.isArray,
      fromArray(name, "number", _.number),
      name,
      value
    )

  def listOfBooleanM[M[_]](
    name: String,
    value: Option[List[Boolean]]
  )(
    implicit M: Applicative[M]
  ): FieldFormlet[M, Option[List[Boolean]]] =
    primitive(
      "array of boolean",
      l => Json.array(l.map(jBool(_)): _*),
      _.isArray,
      fromArray(name, "boolean", _.bool),
      name,
      value
    )

  def stringM[M[_]](
    name: String,
    value: Option[String]
  )(
    implicit M: Applicative[M]
  ): FieldFormlet[M, Option[String]] =
    primitive(
      "string",
      jString(_),
      _.isString,
      ((_: Json).string.map(_.trim)) andThen (a => check(name, "string", a)),
      name,
      value
    )

  def numberM[M[_]](
    name: String,
    value: Option[JsonNumber]
  )(
    implicit M: Applicative[M]
  ): FieldFormlet[M, Option[JsonNumber]] =
    primitive(
      "number",
      jNumber(_),
      _.isNumber,
      ((_: Json).number) andThen (a => check(name, "number", a)),
      name,
      value
    )

  def booleanM[M[_]](
    name: String,
    value: Option[Boolean]
  )(
    implicit M: Applicative[M]
  ): FieldFormlet[M, Option[Boolean]] =
    primitive(
      "boolean",
      jBool(_),
      _.isBool,
      ((_: Json).bool) andThen (a => check(name, "boolean", a)),

      name,
      value
    )

  def label[M[_], A](
    field: FieldFormlet[M, A],
    label: String
  )(
    implicit M: Functor[M]
  ): FieldFormlet[M, A] =
    field.mapView(FieldView.label.set(_, label.some))

  def required[M[_], A](
    field: FieldFormlet[M, Option[A]]
  )(
    implicit M: Functor[M]
  ): FieldFormlet[M, A] =
    field.mapValidation(_.toSuccess(List("This field is required")))

  object Id {
    def listOfString(
      name: String,
      value: Option[List[String]]
    ): IdFieldFormlet[Option[List[String]]] =
      listOfStringM(name, value)

    def listOfNumber(
      name: String,
      value: Option[List[JsonNumber]]
    ): IdFieldFormlet[Option[List[JsonNumber]]] =
      listOfNumberM(name, value)

    def listOfBoolean(
      name: String,
      value: Option[List[Boolean]]
    ): IdFieldFormlet[Option[List[Boolean]]] =
      listOfBooleanM(name, value)

    def string(
      name: String,
      value: Option[String]
    ): IdFieldFormlet[Option[String]] =
      stringM(name, value)

    def number(
      name: String,
      value: Option[JsonNumber]
    ): IdFieldFormlet[Option[JsonNumber]] =
      numberM(name, value)

    def boolean(
      name: String,
      value: Option[Boolean]
    ): IdFieldFormlet[Option[Boolean]] =
      booleanM(name, value)
  }
}
