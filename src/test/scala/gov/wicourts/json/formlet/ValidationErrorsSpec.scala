package gov.wicourts.json.formlet

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scalaz.{Apply, NonEmptyList}
import scalaz.NonEmptyList.nel

import scalaz.{Equal, IList}
import scalaz.syntax.foldable._
import scalaz.syntax.monad._
import scalaz.syntax.monoid._
import scalaz.scalacheck.ScalazProperties._

import org.scalacheck.{Gen, Arbitrary}

import Predef.ArrowAssoc

class ValidationErrorsSpec extends Specification with ScalaCheck {
  "Fields errors" >> {
    "can render themselves to JSON" >> {
      val errors = ValidationErrors.fieldErrors(NonEmptyList("a", "b"))

      errors.toJson.nospaces must_== """["a","b"]"""
    }

    "can be added" >> {
      val e1 = ValidationErrors.fieldErrors(NonEmptyList("a"))
      val e2 = ValidationErrors.fieldErrors(NonEmptyList("b"))

      (e1 |+| e2).toJson.nospaces must_== """["a","b"]"""
    }

    "can be deduped" >> {
      val in = ValidationErrors.fieldErrors(NonEmptyList("a", "b", "c", "b"))
      val out = ValidationErrors.fieldErrors(NonEmptyList("c", "b", "a"))

      Equal[ValidationErrors].equal(ValidationErrors.dedup(in), out) must_== true
    }
  }

  "Object errors" >> {
    val errors = ValidationErrors.objectErrors(List(
      "field1" ->  ValidationErrors.fieldErrors(NonEmptyList("a")),
      "field2" ->  ValidationErrors.fieldErrors(NonEmptyList("b")),
      "obj1" -> ValidationErrors.objectErrors(List(
        "field3" -> ValidationErrors.fieldErrors(NonEmptyList("c"))
      ))
    ))

    "can render themselves to JSON" >> {
      errors.toJson.nospaces must_== """{"field1":["a"],"field2":["b"],"obj1":{"field3":["c"]}}"""
    }

    "can be added" >> {
      val other = ValidationErrors.objectErrors(List(
        "field1" ->  ValidationErrors.fieldErrors(NonEmptyList("d")),
        "field4" ->  ValidationErrors.fieldErrors(NonEmptyList("b")),
        "obj1" -> ValidationErrors.objectErrors(List(
          "field3" -> ValidationErrors.fieldErrors(NonEmptyList("x"))
        ))
      ))

      (errors |+| other).toJson.nospaces must_== """{"field1":["a","d"],"field2":["b"],"obj1":{"field3":["c","x"]},"field4":["b"]}"""
    }

    "can be deduped" >> {
      val in = ValidationErrors.objectErrors(List(
        "field1" -> ValidationErrors.fieldErrors(NonEmptyList("a", "c")),
        "field1" -> ValidationErrors.fieldErrors(NonEmptyList("a")),
        "field2" -> ValidationErrors.fieldErrors(NonEmptyList("b"))
      ))

      val out = ValidationErrors.objectErrors(List(
        "field1" -> ValidationErrors.fieldErrors(NonEmptyList("a", "c")),
        "field2" -> ValidationErrors.fieldErrors(NonEmptyList("b"))
      ))

      Equal[ValidationErrors].equal(ValidationErrors.dedup(in), out) must_== true
    }
  }

  "Array errors" >> {
    val errors = ValidationErrors.arrayErrors(List(
      1 -> ValidationErrors.fieldErrors(NonEmptyList("a")),
      4 -> ValidationErrors.fieldErrors(NonEmptyList("b"))
    ))

    "can render themselves to JSON" >> {
      errors.toJson.nospaces must_== """[null,["a"],null,null,["b"]]"""
    }

    "can be added" >> {
      val other = ValidationErrors.arrayErrors(List(
        1 -> ValidationErrors.fieldErrors(NonEmptyList("a")),
        6 -> ValidationErrors.fieldErrors(NonEmptyList("c"))
      ))
      (errors |+| other).toJson.nospaces must_== """[null,["a","a"],null,null,["b"],null,["c"]]"""
    }

    "can be deduped" >> {
      val in = ValidationErrors.arrayErrors(List(
        1 -> ValidationErrors.fieldErrors(NonEmptyList("a", "c")),
        1 -> ValidationErrors.fieldErrors(NonEmptyList("a")),
        2 -> ValidationErrors.fieldErrors(NonEmptyList("b"))
      ))

      val out = ValidationErrors.arrayErrors(List(
        1 -> ValidationErrors.fieldErrors(NonEmptyList("a", "c")),
        2 -> ValidationErrors.fieldErrors(NonEmptyList("b"))
      ))

      Equal[ValidationErrors].equal(ValidationErrors.dedup(in), out) must_== true
    }
  }

  "Validation errors" >> {
    val in = ValidationErrors.objectErrors(List(
      "something" -> ValidationErrors.fieldErrors(NonEmptyList("a", "b")),
      "other" -> ValidationErrors.arrayErrors(List(
        5 -> ValidationErrors.objectErrors(List(
          "else" -> ValidationErrors.fieldErrors(NonEmptyList("a", "c"))
        ))
      ))
    ))

    "can be collapsed to field errors" >> {
      val out = ValidationErrors.fieldErrors(NonEmptyList("a", "b", "c"))

      Equal[ValidationErrors].equal(ValidationErrors.collapse(in), out) must_== true
    }

    "can be collapsed to a particular name" >> {
      val out = ValidationErrors.objectErrors(List("_error" -> ValidationErrors.fieldErrors(NonEmptyList("a", "b", "c"))))

      Equal[ValidationErrors].equal(ValidationErrors.collapseTo("_error", in), out) must_== true
    }

    "can be folded to a value" >> {
      val fieldError = ValidationErrors.fieldErrors(NonEmptyList("a", "b"))
      val arrayError = ValidationErrors.arrayErrors(List(5 -> fieldError))
      val objectError = ValidationErrors.objectErrors(List("other" -> fieldError))

      def foldToString(errors: ValidationErrors): String =
        errors.fold(
          _.toList.mkString(", "),
          _.map { case (i, v) => s"$i: ${foldToString(v)}" }.mkString("[", ", ", "]"),
          _.map { case (k, v) => s"$k: ${foldToString(v)}" }.mkString("{", ", ", "}")
        )

      foldToString(fieldError) must_== "a, b"
      foldToString(arrayError) must_== "[5: a, b]"
      foldToString(objectError) must_== "{other: a, b}"
    }

    "can be formatted to a String" >> {
      import Predef.augmentString

      val fieldError = ValidationErrors.fieldErrors(NonEmptyList("a", "b"))
      fieldError.pretty must_== "a; b"

      val objectError1 = ValidationErrors.objectErrors(List("first" -> fieldError, "second" -> fieldError))
      objectError1.pretty must_== """- First: a; b
                                    |- Second: a; b""".stripMargin

      val objectError2 = ValidationErrors.objectErrors(List(
        "third" -> fieldError,
        "fourth" -> objectError1,
        "fifth" -> objectError1,
        "sixth" -> fieldError
      ))

      objectError2.pretty must_== """- Third: a; b
                                    |- Fourth:
                                    |  - First: a; b
                                    |  - Second: a; b
                                    |- Fifth:
                                    |  - First: a; b
                                    |  - Second: a; b
                                    |- Sixth: a; b""".stripMargin

      val arrayError1 = ValidationErrors.arrayErrors(List(5 -> fieldError, 6 -> fieldError))

      arrayError1.pretty must_== """- 5: a; b
                                   |- 6: a; b""".stripMargin

      val arrayError2 = ValidationErrors.arrayErrors(List(5 -> objectError1))

      arrayError2.pretty must_== """- 5:
                                   |  - First: a; b
                                   |  - Second: a; b""".stripMargin

      val objectError3 = ValidationErrors.objectErrors(List(
        "seventhName" -> fieldError,
        "eighthName" -> arrayError2,
        "ninthName" -> fieldError,
        "tenthName" -> objectError1
      ))

      objectError3.pretty must_== """- Seventh name: a; b
                                    |- Eighth name:
                                    |  - 5:
                                    |    - First: a; b
                                    |    - Second: a; b
                                    |- Ninth name: a; b
                                    |- Tenth name:
                                    |  - First: a; b
                                    |  - Second: a; b""".stripMargin
    }
  }

  "Type class laws" >> {
    implicit val arbitraryValidationErrors: Arbitrary[ValidationErrors] = Arbitrary {
      def genValidationErrors(arraySize: Int): Gen[ValidationErrors] =
        Gen.frequency[ValidationErrors](
          1 -> ^(Gen.alphaStr, Gen.listOf(Gen.alphaStr))((h, t) => FieldErrors(nel(h, IList.fromList(t)))),
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
