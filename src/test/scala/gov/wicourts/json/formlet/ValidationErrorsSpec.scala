package gov.wicourts.json.formlet

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scalaz.{Apply, NonEmptyList, Monoid}
import scalaz.NonEmptyList.nel

import argonaut._
import argonaut.Argonaut.jNull

import scalaz.Equal
import scalaz.syntax.equal._
import scalaz.syntax.monad._
import scalaz.syntax.monoid._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

import org.scalacheck.{Gen, Arbitrary}

class ValidationErrorsSpec extends Specification with ScalaCheck {
  "Fields errors" >> {
    "can render themselves to JSON" >> {
      val errors = ValidationErrors.field(NonEmptyList("a", "b"))

      errors.toJson.nospaces must_== """["a","b"]"""
    }

    "can be added" >> {
      val e1 = ValidationErrors.field(NonEmptyList("a"))
      val e2 = ValidationErrors.field(NonEmptyList("b"))

      (e1 |+| e2).toJson.nospaces must_== """["a","b"]"""
    }

    "can be deduped" >> {
      val in = ValidationErrors.field(NonEmptyList("a", "b", "c", "b"))
      val out = ValidationErrors.field(NonEmptyList("c", "b", "a"))

      Equal[ValidationErrors].equal(ValidationErrors.dedup(in), out) must_== true
    }
  }

  "Object errors" >> {
    val errors = ValidationErrors.obj(List(
      "field1" ->  ValidationErrors.field(NonEmptyList("a")),
      "field2" ->  ValidationErrors.field(NonEmptyList("b")),
      "obj1" -> ValidationErrors.obj(List(
        "field3" -> ValidationErrors.field(NonEmptyList("c"))
      ))
    ))

    "can render themselves to JSON" >> {
      errors.toJson.nospaces must_== """{"field1":["a"],"field2":["b"],"obj1":{"field3":["c"]}}"""
    }

    "can be added" >> {
      val other = ValidationErrors.obj(List(
        "field1" ->  ValidationErrors.field(NonEmptyList("d")),
        "field4" ->  ValidationErrors.field(NonEmptyList("b")),
        "obj1" -> ValidationErrors.obj(List(
          "field3" -> ValidationErrors.field(NonEmptyList("x"))
        ))
      ))

      (errors |+| other).toJson.nospaces must_== """{"field1":["a","d"],"field2":["b"],"obj1":{"field3":["c","x"]},"field4":["b"]}"""
    }

    "can be deduped" >> {
      val in = ValidationErrors.obj(List(
        "field1" -> ValidationErrors.field(NonEmptyList("a", "c")),
        "field1" -> ValidationErrors.field(NonEmptyList("a")),
        "field2" -> ValidationErrors.field(NonEmptyList("b"))
      ))

      val out = ValidationErrors.obj(List(
        "field1" -> ValidationErrors.field(NonEmptyList("a", "c")),
        "field2" -> ValidationErrors.field(NonEmptyList("b"))
      ))

      Equal[ValidationErrors].equal(ValidationErrors.dedup(in), out) must_== true
    }
  }

  "Array errors" >> {
    val errors = ValidationErrors.array(List(
      1 -> ValidationErrors.field(NonEmptyList("a")),
      4 -> ValidationErrors.field(NonEmptyList("b"))
    ))

    "can render themselves to JSON" >> {
      errors.toJson.nospaces must_== """[null,["a"],null,null,["b"]]"""
    }

    "can be added" >> {
      val other = ValidationErrors.array(List(
        1 -> ValidationErrors.field(NonEmptyList("a")),
        6 -> ValidationErrors.field(NonEmptyList("c"))
      ))
      (errors |+| other).toJson.nospaces must_== """[null,["a","a"],null,null,["b"],null,["c"]]"""
    }

    "can be deduped" >> {
      val in = ValidationErrors.array(List(
        1 -> ValidationErrors.field(NonEmptyList("a", "c")),
        1 -> ValidationErrors.field(NonEmptyList("a")),
        2 -> ValidationErrors.field(NonEmptyList("b"))
      ))

      val out = ValidationErrors.array(List(
        1 -> ValidationErrors.field(NonEmptyList("a", "c")),
        2 -> ValidationErrors.field(NonEmptyList("b"))
      ))

      Equal[ValidationErrors].equal(ValidationErrors.dedup(in), out) must_== true
    }
  }

  "Validation errors" >> {
    "can be collapsed be field errors" >> {
      val in = ValidationErrors.obj(List(
        "something" -> ValidationErrors.field(NonEmptyList("a", "b")),
        "other" -> ValidationErrors.array(List(
          5 -> ValidationErrors.obj(List(
            "else" -> ValidationErrors.field(NonEmptyList("a", "c"))
          ))
        ))
      ))

      val out = ValidationErrors.field(NonEmptyList("a", "b", "c"))

      Equal[ValidationErrors].equal(ValidationErrors.collapse(in), out) must_== true
    }
  }

  "Type class laws" >> {
    implicit val arbitraryValidationErrors: Arbitrary[ValidationErrors] = Arbitrary {
      def genValidationErrors(arraySize: Int): Gen[ValidationErrors] =
        Gen.frequency[ValidationErrors](
          1 -> ^(Gen.alphaStr, Gen.listOf(Gen.alphaStr))((h, t) => FieldErrors(nel(h, t))),
          1 -> Gen.listOfN(arraySize, Apply[Gen].tuple2(Gen.alphaStr, genValidationErrors(arraySize/2))).map(l =>
            ObjectErrors(l)
          ),
          1 -> Gen.listOfN(arraySize, ^(Gen.choose(1, 10), genValidationErrors(arraySize/2))((_, _))).map(l =>
            ArrayErrors(l)
          )
        )

      genValidationErrors(4)
    }

    "Monoid" >> {
      monoid.laws[ValidationErrors]
    }

    "Equal" >> {
      equal.laws[ValidationErrors]
    }
  }
}
