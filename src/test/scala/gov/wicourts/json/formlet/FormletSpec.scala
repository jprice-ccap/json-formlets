package gov.wicourts.json.formlet

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scalaz.{Equal, Apply, Applicative}
import scalaz.Id.Id
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalazProperties._
import scalaz.std.string._
import scalaz.syntax.monad._
import scalaz.syntax.validation._

import org.scalacheck.{Gen, Arbitrary}

class FormletSpec extends Specification with ScalaCheck {
  "Formlet" >> {
    "Type class laws" >> {
      "Applicative" >> {
        type SampleFormlet[A] = Formlet[Id, Int, String, A, String]

        def intFormlet[A](f: Int => A): Arbitrary[SampleFormlet[A]] = Arbitrary(
          Gen.frequency(
            1 -> Gen.alphaStr.map(s => Formlet(i => (f(i).success[String], s).point[Id])),
            1 -> Apply[Gen].tuple2(Gen.alphaStr, Gen.alphaStr).map { case (s1, s2) =>
              Formlet(i => (s1.failure[A], s2).point[Id])
            }
          )
        )

        val sampleEqual: Equal[SampleFormlet[Int]] = Equal.equal((a1, a2) =>
          a1.run(99) === a2.run(99)
        )

        applicative.laws[SampleFormlet](
          Applicative[SampleFormlet],
          intFormlet((i: Int) => i*2),
          intFormlet[Int => Int](i => i2 => i + i2 + 1),
          sampleEqual
        )
      }
    }
  }
}
