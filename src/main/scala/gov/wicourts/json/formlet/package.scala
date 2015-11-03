package gov.wicourts.json

import argonaut.Json

package object formlet {
  type FieldFormlet[A] = Formlet[JsonObjectBuilder, A, FieldView]

  type ObjectFormlet[A] = Formlet[JsonObjectBuilder, A, JsonObjectBuilder]

  object syntax extends ToFieldFormletOps
}
