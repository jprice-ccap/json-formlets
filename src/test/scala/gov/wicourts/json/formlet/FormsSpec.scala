package gov.wicourts.json.formlet

import org.specs2.mutable.Specification

import scalaz.std.list._
import scalaz.syntax.applicative._
import scalaz.syntax.std.option._
import scalaz.syntax.validation._

import argonaut._
import argonaut.Argonaut.jNull

import gov.wicourts.json.formlet.Forms._
import gov.wicourts.json.formlet.Forms.Id._
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

      result.toString must contain("Field nameL must be a(n) string")
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

    "can be required" >> {
      val (a, _) = string("nameL", None).required.run(jNull)

      a must_== List("This field is required").failure
    }

    "can be validated" >> {
      val f = number("count", None)
        .required
        .validate(
          _.success.ensure(List("count must be bigger than 7"))(_.truncateToInt > 7),
          _.success.ensure(List("count must be less than 5"))(_.truncateToInt < 5)
        )
      val (result, _) = f.run(parse("""{"count":6}"""))

      result must_== List("count must be bigger than 7", "count must be less than 5").failure
    }
  }

  "A string array form" >> {
    "should be able to render its value" >> {
      val (_, view) = listOfString("colors", List("red", "blue", "green").some).run(jNull)
      view.toJson.nospaces must_== """{"colors":{"value":["red","blue","green"]}}"""
    }

    "should be able to extract its value" >> {
      val (result, _) = listOfString("colors", None).run(
        parse("""{"colors":["red","green"]}""")
      )

      result must_== List("red", "green").some.success
    }

    "should fail if property is not an array" >> {
      val (result, _) = listOfString("colors", None).run(
        parse("""{"colors":1}""")
      )

      result must_== List("Field colors must be a(n) array of string").failure
    }

    "should fail if array does not contain required type" >> {
      val (result, _) = listOfString("colors", None).run(
        parse("""{"colors":["red", 1]}""")
      )

      result must_== List("Expected a string when processing field colors").failure
    }
  }

  "A composite form example" >> {
    case class FullName(nameF: Option[String], nameL: Option[String])

    def fullNameForm(fullName: FullName): IdObjectFormlet[FullName] =
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

    "can be nested" >> {
      "and should be able to render initial data" >> {
        val fullName = FullName("Jack".some, "Sprat".some)
        val (_, view) = nested("fullName", fullNameForm(fullName)).run(jNull)

        val json = """{"fullName":{"nameL":{"value":"Sprat"},"nameF":{"value":"Jack"}}}"""
        view.toJson.nospaces must_== json
      }

      "and should be able to extract data" >> {
        val fullName = FullName(None, None)
        val json = """{"fullName":{"nameL":"Sprat","nameF":"Jack"}}"""
        val (result, _) = nested("fullName", fullNameForm(fullName)).run(parse(json))

        result must_== FullName("Jack".some, "Sprat".some).success
      }
    }
  }
}
