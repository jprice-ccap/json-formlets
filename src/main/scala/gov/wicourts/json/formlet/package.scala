package gov.wicourts.json

import argonaut.Json

import scalaz.Id.Id

import scala.language.higherKinds

package object formlet {
  type IdFieldFormlet[A] = Formlet[Id, Json, List[String], A, FieldView]
  type IdObjectFormlet[A] = Formlet[Id, Json, JsonObjectBuilder, A, JsonObjectBuilder]

  type FieldFormlet[M[_], A] = Formlet[M, Json, List[String], A, FieldView]
  type ObjectFormlet[M[_], A] = Formlet[M, Json, JsonObjectBuilder, A, JsonObjectBuilder]

  type JsonFormlet[M[_], E, A, V] = Formlet[M, Json, E, A, V]

  object syntax extends ToFieldFormletOps
}
