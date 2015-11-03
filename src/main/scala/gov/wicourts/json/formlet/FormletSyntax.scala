package gov.wicourts.json.formlet

import scala.language.implicitConversions

class FieldFormletOps[A](self: FieldFormlet[A]) {
  def row: ObjectFormlet[A] = Forms.row[A](self)
  def label(s: String): FieldFormlet[A] = Forms.label(self, s)
}

trait ToFieldFormletOps {
  implicit def FieldFormletToFieldFormletOps[A](v: FieldFormlet[A]): FieldFormletOps[A] =
    new FieldFormletOps[A](v)
}
