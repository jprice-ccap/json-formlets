package gov.wicourts.json

import argonaut.Json

import scalaz.Id.Id

package object formlet {
  type FieldFormlet[A] = Formlet[Id, Json, List[String], A, FieldView]

  type ObjectFormlet[A] = Formlet[Id, Json, JsonObjectBuilder, A, JsonObjectBuilder]

  object syntax extends ToFieldFormletOps
}
