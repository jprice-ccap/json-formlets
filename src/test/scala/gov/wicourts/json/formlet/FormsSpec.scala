package gov.wicourts.json.formlet

import org.specs2.mutable.Specification

import scalaz.syntax.std.option._
import scalaz.syntax.validation._
import scalaz.syntax.applicative._

import argonaut._
import argonaut.Argonaut.jNull

import gov.wicourts.json.formlet.Forms._
import gov.wicourts.json.formlet.syntax._

class FormsSpec extends Specification {
  private def parse(s: String): Json =
    Parse.parseOption(s).getOrElse(throw new Exception("Unexpected parse failure"))

  "A string form" >> {
    "should be able to render its value" >> {
      val (_, view) = string("nameL", "Smith".some).run(jNull)
      view.toJson.nospaces must_== """{"nameL":{"value":"Smith"}}"""
    }

    "should be able to extract its value" >> {
      val (result, _) = string("nameL", None).run(
        parse("""{"nameL":"Smith"}""")
      )

      result must_== "Smith".some.success
    }

    "should fail with an error message if value exists, but is not a string" >> {
      val (result, _) = string("nameL", None).run(
        parse("""{"nameL":1}""")
      )

      result.toString must contain("Field nameL must be a string")
    }

    "should treat null as empty (good idea?)" >> {
      val (result, _) = string("nameL", None).run(
        parse("""{"nameL":null}""")
      )

      result must_== None.success
    }

    "should trim spaces from the result" >> {
      val (result, _) = string("nameL", None).run(
        parse("""{"nameL":" Smith  "}""")
      )

      result must_== "Smith".some.success
    }

    "should omit the value from the rendered view if value is not defined" >> {
      val (_, view) = string("nameL", None).run(jNull)

      view.toJson.nospaces must_== "{}"
    }

    "can be assigned a label" >> {
      val (_, view) = string("nameL", None).label("Last name").run(jNull)

      view.toJson.nospaces must_== """{"nameL":{"metadata":{"label":"Last name"}}}"""
    }
  }

  "A composite form example" >> {
    case class FullName(nameF: Option[String], nameL: Option[String])

    def fullNameForm(fullName: FullName): ObjectFormlet[FullName] =
      ^(
        string("nameF", fullName.nameF).row,
        string("nameL", fullName.nameL).row
      )(FullName.apply _)

    "should be able to render initial data" >> {
      val (_, view) = fullNameForm(FullName("Jack".some, "Sprat".some)).run(jNull)

      view.toJson.nospaces must_== """{"nameL":{"value":"Sprat"},"nameF":{"value":"Jack"}}"""
    }

    "should be able to extract data" >> {
      val (result, view) = fullNameForm(FullName(None, None)).run(
        parse("""{"nameL":"Sprat","nameF":"Jack"}""")
      )

      view.toJson.nospaces must_== """{"nameL":{"value":"Sprat"},"nameF":{"value":"Jack"}}"""
      result must_== FullName("Jack".some, "Sprat".some).success
    }
  }
}
