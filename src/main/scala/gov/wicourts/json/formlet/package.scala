package gov.wicourts.json

import argonaut.Json

import scalaz.Id.Id

import scala.language.higherKinds

import scalaz.NonEmptyList

package object formlet {
  type IdFieldFormlet[A] = Formlet[Id, Json, NonEmptyList[String], A, FieldView]
  type IdObjectFormlet[A] = Formlet[Id, Json, ValidationErrors, A, JsonObjectBuilder]

  type FieldFormlet[M[_], A] = Formlet[M, Json, NonEmptyList[String], A, FieldView]
  type ObjectFormlet[M[_], A] = Formlet[M, Json, ValidationErrors, A, JsonObjectBuilder]

  type JsonFormlet[M[_], E, A, V] = Formlet[M, Json, E, A, V]

  object syntax extends ToFieldFormletOps with ToObjectFormletOps
}
