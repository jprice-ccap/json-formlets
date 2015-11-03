package gov.wicourts.json.formlet

import org.specs2.mutable.Specification

import scalaz.syntax.std.option._
import scalaz.syntax.validation._
import scalaz.syntax.applicative._

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

  "A composite form example" >> {
    case class FullName(nameF: Option[String], nameL: Option[String])

    def fullNameForm(fullName: FullName): ObjectFormlet[FullName] =
      ^(
        Forms.string("nameF", fullName.nameF),
        Forms.string("nameL", fullName.nameL)
      )(FullName.apply _)

    "should be able to render initial data" >> {
      val (_, view) = fullNameForm(FullName("Jack".some, "Sprat".some)).run(jNull)

      view.view.toJson.nospaces must_== """{"nameL":"Sprat","nameF":"Jack"}"""
    }

    "should be able to extract data" >> {
      val (result, view) = fullNameForm(FullName(None, None)).run(
        parse("""{"nameL":"Sprat","nameF":"Jack"}""")
      )

      view.view.toJson.nospaces must_== """{"nameL":"Sprat","nameF":"Jack"}"""
      result must_== FullName("Jack".some, "Sprat".some).success
    }
  }
}
