package gov.wicourts.json

import argonaut.Cursor

import scalaz.Id.Id

import scala.language.higherKinds

import scalaz.NonEmptyList

package object formlet {

  type JsonFormlet[M[_], E, A, V] = Formlet[M, Option[Cursor], E, A, V]

  type FieldFormlet[M[_], A] = JsonFormlet[M, NonEmptyList[String], A, FieldView]
  type ObjectFormlet[M[_], A] = JsonFormlet[M, ValidationErrors, A, JsonObjectBuilder]

  type IdFieldFormlet[A] = FieldFormlet[Id, A]
  type IdObjectFormlet[A] = ObjectFormlet[Id, A]


  object syntax extends ToFieldFormletOps with ToObjectFormletOps
}
