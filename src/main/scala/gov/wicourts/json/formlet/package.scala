package gov.wicourts.json

import argonaut.Json

package object formlet {
  type ObjectFormlet[A] = Formlet[JsonObjectBuilder, A, JsonObjectBuilder]

  def row(name: String, json: Json): JsonObjectBuilder =
    new JsonObjectBuilder(List((name, json)))

  def item(json: Json): JsonArrayBuilder =
    new JsonArrayBuilder(List(json))
}
