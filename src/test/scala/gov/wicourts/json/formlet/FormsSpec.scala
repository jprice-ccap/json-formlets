package gov.wicourts.json.formlet

import org.specs2.mutable.Specification

import scalaz.syntax.std.option._
import scalaz.syntax.validation._

import argonaut._
import argonaut.Argonaut.jNull

class FormsSpec extends Specification {
  private def parse(s: String): Json =
    Parse.parseOption(s).getOrElse(throw new Exception("Unexpected parse failure"))

  "A string form" >> {
    "should be able to render its value" >> {
      val (_, view) = Forms.string("nameL", "Smith".some).run(jNull.cursor)
      view.toJson.nospaces must_== """{"nameL":"Smith"}"""
    }

    "should be able to extract its value" >> {
      val (result, _) = Forms.string("nameL", None).run(
        parse("""{"nameL":"Smith"}""").cursor
      )

      result must_== "Smith".some.success
    }
  }
}
