package gov.wicourts.json.formlet

import scalaz.{Applicative, Monoid, Semigroup, Validation}

import scalaz.syntax.applicative._
import scalaz.syntax.monoid._
import scalaz.syntax.validation._

case class Formlet[I, E, A, V](run: I => (Validation[E, A], V)) {
  def map[B](f: A => B): Formlet[I, E, B, V] =
    Formlet(c => {
      val (result, view) = run(c)
      (result map f, view)
    })

  def ap[B](f: => Formlet[I, E, A => B, V])(implicit E: Semigroup[E], V: Monoid[V]): Formlet[I, E, B, V] =
    Formlet(c => {
      val (a, v1) = this.run(c)
      val (ff, v2) = f.run(c)
      (a <*> ff, v1 |+| v2)
    })

  def mapView[U](f: V => U): Formlet[I, E, A, U] =
    Formlet(c => {
      val (a, v) = run(c)
      (a, f(v))
    })
}

object Formlet {
  implicit def formletApplicative[
    I,
    E : Semigroup,
    V : Monoid
  ]: Applicative[Formlet[I, E, ?, V]] =
    new Applicative[Formlet[I, E, ?, V]] {
      override def map[A, B](a: Formlet[I, E, A, V])(f: A => B): Formlet[I, E, B, V] =
        a map f

      override def point[A](a: => A): Formlet[I, E, A, V] =
        Formlet[I, E, A, V](_ => (a.success, Monoid[V].zero))

      override def ap[A, B](fa: => Formlet[I, E, A, V])(f: => Formlet[I, E, A => B, V]): Formlet[I, E, B, V] =
        fa ap f
    }
}
