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
      val (_, view) = Forms.string("nameL", "Smith".some).run(jNull)
      view.view.toJson.nospaces must_== """{"nameL":"Smith"}"""
    }

    "should be able to extract its value" >> {
      val (result, _) = Forms.string("nameL", None).run(
        parse("""{"nameL":"Smith"}""")
      )

      result must_== "Smith".some.success
    }

    "should fail with an error message if value exists, but is not a string" >> {
      val (result, _) = Forms.string("nameL", None).run(
        parse("""{"nameL":1}""")
      )

      result.toString must contain("Field nameL must be a string")
    }

    "should treat null as empty (good idea?)" >> {
      val (result, _) = Forms.string("nameL", None).run(
        parse("""{"nameL":null}""")
      )

      result must_== None.success
    }

    "should trim spaces from the result" >> {
      val (result, _) = Forms.string("nameL", None).run(
        parse("""{"nameL":" Smith  "}""")
      )

      result must_== "Smith".some.success
    }

    "should omit the value from the rendered view if value is not defined" >> {
      val (_, view) = Forms.string("nameL", None).run(jNull)

      view.view.toJson.nospaces must_== "{}"
    }
  }
}
