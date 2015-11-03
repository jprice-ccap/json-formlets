package gov.wicourts.json.formlet

import scalaz.Monoid
import scalaz.std.map._
import scalaz.syntax.monoid._

case class JsonView(view: JsonObjectBuilder, metadata: Map[String, JsonObjectBuilder])

object JsonView {
  implicit val monoid: Monoid[JsonView] = new Monoid[JsonView] {
    def zero: JsonView = JsonView(
      Monoid[JsonObjectBuilder].zero,
      Monoid[Map[String, JsonObjectBuilder]].zero
    )

    def append(f1: JsonView, f2: => JsonView): JsonView =
      JsonView(
        f1.view |+| f2.view,
        f1.metadata |+| f2.metadata
      )
  }
}
