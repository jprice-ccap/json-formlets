package gov.wicourts.json.formlet

import scalaz.{\/, NonEmptyList, Monoid}

import argonaut.Json
import argonaut.Json.{jNumber, jString}

import scalaz.Equal
import scalaz.State._

import scalaz.std.anyVal._
import scalaz.std.list._
import scalaz.std.string._
import scalaz.std.tuple._

import scalaz.syntax.equal._
import scalaz.syntax.monoid._
import scalaz.syntax.traverse._

sealed trait ValidationErrors {
  def toJson: Json
}

private case class FieldErrors(errors: NonEmptyList[String]) extends ValidationErrors {
  def toJson: Json = Json.array(errors.map(jString(_)).toList: _*)
}

private case class ObjectErrors(errors: List[(String, ValidationErrors)]) extends ValidationErrors {
  def toJson: Json = Json.obj(errors.map { case (n, e) => (n, e.toJson) }: _*)
}

private case class ArrayErrors(errors: List[(Int, ValidationErrors)]) extends ValidationErrors {
  def toJson: Json = {
    val jsonErrors = errors
      .sortBy(_._1)
      .foldLeft(Vector[Json]()) { case (l, (n, e)) => l.padTo(n, Json.jNull) :+ e.toJson }
    Json.array(jsonErrors: _*)
  }
}

object ValidationErrors {
  private [formlet] def field(errors: NonEmptyList[String]): ValidationErrors =
    FieldErrors(errors)

  private [formlet] def obj(errors: List[(String, ValidationErrors)]): ValidationErrors =
    ObjectErrors(errors)

  private [formlet] def array(errors: List[(Int, ValidationErrors)]): ValidationErrors =
    ArrayErrors(errors)

  def dedup(errors: ValidationErrors): ValidationErrors = {
    def dedup_[A](l: List[(A, ValidationErrors)]): List[(A, ValidationErrors)] =
      l.groupBy(_._1).toList.map { case (s, vs) =>
        (s, dedup(vs.map(_._2).suml))
      }

    errors match {
      case FieldErrors(l) => FieldErrors(l.distinct)
      case ObjectErrors(l) => ObjectErrors(dedup_(l))
      case ArrayErrors(l) => ArrayErrors(dedup_(l))
    }
  }

  def string(name: String, error: String): ValidationErrors =
    ObjectErrors(List((name, FieldErrors(NonEmptyList(error)))))

  def list(name: String, errors: NonEmptyList[String]): ValidationErrors =
    ObjectErrors(List((name, FieldErrors(errors))))

  def relabel(errors: ValidationErrors, from: String, to: String): ValidationErrors =
    errors match {
      case ObjectErrors(errors) => ObjectErrors(
        errors.map { case (k, v) => if (k === from) (to, v) else (k, v) }
      )
      case v => v
    }

  implicit val validationErrorsEqual: Equal[ValidationErrors] = Equal.equal((a1, a2) => {
    (a1, a2) match {
      case (FieldErrors(l1), FieldErrors(l2)) => l1.toList.sorted === l2.toList.sorted
      case (ArrayErrors(l1), ArrayErrors(l2)) => l1.sortBy(_._1) === l2.sortBy(_._1)
      case (ObjectErrors(l1), ObjectErrors(l2)) => l1.sortBy(_._1) === l2.sortBy(_._1)
      case (_, _) => false
    }
  })

  implicit val validationErrorsMonoid: Monoid[ValidationErrors] = new Monoid[ValidationErrors] {
    def zero: ValidationErrors = ObjectErrors(Nil)

    private def merge[A : Equal](
      e1: List[(A, ValidationErrors)],
      e2: List[(A, ValidationErrors)]
    ): List[(A, ValidationErrors)] = {
      val (remaining, merged) =
        e1.traverseS { case (a, errors) =>
          for {
            s <- get[List[(A, ValidationErrors)]]
            (ours, others) = s.partition { case (a1, _) => a1 === a }
            _ <- put(others)
          } yield {
            import scalaz.syntax.foldable1._

            (a, NonEmptyList(errors, ours.map { case (_, e) => e }: _*).suml1)
          }
        }.run(e2)
      merged ++ remaining
    }

    def append(f1: ValidationErrors, f2: => ValidationErrors): ValidationErrors = {
      (f1, f2) match {
        case (FieldErrors(l1), FieldErrors(l2)) => FieldErrors(l1 |+| l2)
        case (ObjectErrors(e1), ObjectErrors(e2)) => ObjectErrors(merge(e1, e2))
        case (ArrayErrors(e1), ArrayErrors(e2)) => ArrayErrors(merge(e1, e2))
        case (_, f@FieldErrors(_)) => f
        case (f@FieldErrors(_), _) => f
        case (_, a@ArrayErrors(_)) => a
        case (a@ArrayErrors(_), _) => a
      }
    }
  }
}
