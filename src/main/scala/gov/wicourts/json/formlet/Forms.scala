package gov.wicourts.json.formlet

import argonaut._
import argonaut.Argonaut.{jString, jNull}

import scalaz.std.option._
import scalaz.syntax.bind._
import scalaz.syntax.std.option._
import scalaz.syntax.validation._

object Forms {
  def string(name: String, value: Option[String]): ObjectFormlet[Option[String]] =
    Formlet(c => {
      (
        (c.downField(name) ∘ (_.focus) >>= (_.string)).success[JsonObjectBuilder],
        row(name, value ∘ (jString(_)) | jNull)
      )
    })
}
