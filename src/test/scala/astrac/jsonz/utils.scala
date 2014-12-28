package astrac.jsonz

import scalaz._
import Scalaz._

case class Baz(qux: Int)
case class Foobar(foo: Int, bar: Boolean, baz: Baz)

object Utils {
  def getPropertyErrors(v: ValidationNel[ValidationError, _]) =
    v.fold(_.toList, _ => Nil).map { case ValidationError(err, path) => path.mkString("/") }
}
