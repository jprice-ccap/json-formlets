package gov.wicourts.json.formlet

import scala.language.implicitConversions
import scala.language.higherKinds

import scalaz.Functor

class FieldFormletOps[M[_], A](self: FieldFormlet[M, A])(implicit M: Functor[M]) {
  def obj: ObjectFormlet[M, A] = Forms.obj[M, A](self)
  def label(s: String): FieldFormlet[M, A] = Forms.label(self, s)

  def required[B](implicit ev: A <:< Option[B]): FieldFormlet[M, B] =
    Forms.required(self.map(a => a: Option[B]))
}

class IdFieldFormletOps[A](self: IdFieldFormlet[A]) {
  def obj: IdObjectFormlet[A] = Forms.obj(self)
  def label(s: String): IdFieldFormlet[A] = Forms.label(self, s)

  def required[B](implicit ev: A <:< Option[B]): IdFieldFormlet[B] =
    Forms.required(self.map(a => a: Option[B]))
}

trait ToFieldFormletOps0 {
  implicit def FieldFormletToFieldFormletOps[M[_], A](
    v: FieldFormlet[M, A]
  )(
    implicit M: Functor[M]
  ): FieldFormletOps[M, A] =
    new FieldFormletOps[M, A](v)
}

trait ToFieldFormletOps extends ToFieldFormletOps0 {
  implicit def IdFieldFormletToIdFieldFormletOps[A](v: IdFieldFormlet[A]): IdFieldFormletOps[A] =
    new IdFieldFormletOps(v)
}
