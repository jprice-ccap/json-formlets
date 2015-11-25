package gov.wicourts.json.formlet

import scalaz._

import scalaz.NonEmptyList.nel

import scalaz.std.function._
import scalaz.std.list._
import scalaz.syntax.applicative._
import scalaz.syntax.monoid._
import scalaz.syntax.traverse1._
import scalaz.syntax.validation._

import scala.language.higherKinds

case class Formlet[M[_], I, E, A, V](run: I => M[(Validation[E, A], V)]) {
  def eval(i: I)(implicit M: Functor[M]): M[Validation[E, A]] =
    M.map(run(i))(_._1)

  def view(i: I)(implicit M: Functor[M]): M[V] =
    M.map(run(i))(_._2)

  def map[B](f: A => B)(implicit M: Functor[M]): Formlet[M, I, E, B, V] =
    mapResult((result, view) => (result map f, view))

  def ap[B](
    f: => Formlet[M, I, E, A => B, V]
  )(
    implicit E: Semigroup[E], V: Monoid[V], M: Applicative[M]
  ): Formlet[M, I, E, B, V] =
    Formlet(c =>
      M.apply2(this.run(c), f.run(c)) { case ((a, v1), (ff, v2)) =>
        (a <*> ff, v1 |+| v2)
      }
    )

  def mapView[U](f: V => U)(implicit M: Functor[M]): Formlet[M, I, E, A, U] =
    mapResult((result, view) => (result, f(view)))

  def mapResult[EE, AA, W](
    f: (Validation[E, A], V) => (Validation[EE, AA], W)
  )(
    implicit M: Functor[M]
  ): Formlet[M, I, EE, AA, W] =
    Formlet(c => M.map(run(c))(f.tupled))

  def mapValidation[B](
    f: A => Validation[E, B]
  )(
    implicit M: Functor[M]
  ): Formlet[M, I, E, B, V] = {
    // XXX Import here to prevent conflicts with applicative syntax
    import scalaz.syntax.bind._
    mapResult((a, v) => ((a.disjunction >>= (f(_).disjunction)).validation, v))
  }

  def validate[B](
    h: A => Validation[E, B],
    t: (A => Validation[E, B])*
  )(
    implicit E: Semigroup[E], M: Applicative[M]
  ): Formlet[M, I, E, B, V] = {
    val X = Applicative[A => ?].compose[Validation[E, ?]]
    val f = X.sequence(nel(h, t.toList))
    mapValidation(f).map(_.head)
  }

  def mapResultM[EE, AA, W](
    f: (Validation[E, A], V) => M[(Validation[EE, AA], W)]
  )(
    implicit M: Bind[M]
  ): Formlet[M, I, EE, AA, W] =
    Formlet(c => M.bind(run(c))(f.tupled))

  def mapValidationM[B](
    f: A => M[Validation[E, B]]
  )(
    implicit M: Monad[M]
  ): Formlet[M, I, E, B, V] = {
    // XXX Import here to prevent conflicts with applicative syntax
    import scalaz.syntax.bind._
    mapResultM((a, v) =>
      EitherT(M.point(a.disjunction))
        .flatMapF(f(_) ∘ (_.disjunction))
        .run
        .map(r => (r.validation, v))
    )
  }

  def validateM[B](
    h: A => M[Validation[E, B]],
    t: (A => M[Validation[E, B]])*
  )(
    implicit E: Semigroup[E], M: Monad[M]
  ): Formlet[M, I, E, B, V] = {
    val X = Applicative[A => ?].compose[M].compose[Validation[E, ?]]
    val f = X.sequence(nel(h, t.toList))
    mapValidationM(f).map(_.head)
  }

}

object Formlet {
  implicit def formletApplicative[
    M[_] : Applicative,
    I,
    E : Semigroup,
    V : Monoid
  ]: Applicative[Formlet[M, I, E, ?, V]] =
    new Applicative[Formlet[M, I, E, ?, V]] {
      override def map[A, B](a: Formlet[M, I, E, A, V])(f: A => B): Formlet[M, I, E, B, V] =
        a map f

      override def point[A](a: => A): Formlet[M, I, E, A, V] =
        Formlet[M, I, E, A, V](_ => Applicative[M].point((a.success, Monoid[V].zero)))

      override def ap[A, B](
        fa: => Formlet[M, I, E, A, V]
      )(
        f: => Formlet[M, I, E, A => B, V]
      ): Formlet[M, I, E, B, V] =
        fa ap f
    }
}
