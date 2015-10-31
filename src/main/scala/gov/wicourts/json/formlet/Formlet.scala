package gov.wicourts.json.formlet

import argonaut.Cursor

import scalaz.{Applicative, Monoid, Semigroup, Validation}

import scalaz.syntax.applicative._
import scalaz.syntax.monoid._
import scalaz.syntax.validation._

case class Formlet[E : Semigroup, A, M <: JsonBuilder : Monoid](
  run: Cursor => (Validation[E, A], M)
) {
  def map[B](f: A => B): Formlet[E, B, M] =
    Formlet(c => {
      val (result, view) = run(c)
      (result map f, view)
    })

  def ap[B](f: => Formlet[E, A => B, M]): Formlet[E, B, M] =
    Formlet(c => {
      val (a, v1) = this.run(c)
      val (ff, v2) = f.run(c)
      (a <*> ff, v1 |+| v2)
    })
}

object Formlet {
  implicit def formletApplicative[
    E : Semigroup,
    M <: JsonBuilder : Monoid
  ]: Applicative[({ type l[a] = Formlet[E, a, M] })#l] =
    new Applicative[({ type l[a] = Formlet[E, a, M] })#l] {
      override def map[A, B](a: Formlet[E, A, M])(f: A => B): Formlet[E, B, M] =
        a map f

      override def point[A](a: => A): Formlet[E, A, M] =
        Formlet[E, A, M](_ => (a.success, Monoid[M].zero))

      override def ap[A, B](fa: => Formlet[E, A, M])(f: => Formlet[E, A => B, M]): Formlet[E, B, M] =
        fa ap f
    }
}
