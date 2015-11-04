package gov.wicourts.json.formlet

import scala.language.implicitConversions

class FieldFormletOps[A](self: FieldFormlet[A]) {
  def row: ObjectFormlet[A] = Forms.row[A](self)
  def label(s: String): FieldFormlet[A] = Forms.label(self, s)

  def required[B](implicit ev: A <:< Option[B]): FieldFormlet[B] =
    Forms.required(self.map(a => a: Option[B]))
}

trait ToFieldFormletOps {
  implicit def FieldFormletToFieldFormletOps[A](v: FieldFormlet[A]): FieldFormletOps[A] =
    new FieldFormletOps[A](v)
}
