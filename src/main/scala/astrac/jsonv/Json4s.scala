package astrac.jsonv

import org.json4s._
import scala.reflect.ClassTag
import scalaz._
import Scalaz._

object Json4s {
  type ErrorFormatter = Formats => ErrorList => JValue
  val Errors = scala.reflect.classTag[ErrorList]

  val defaultErrorFormatter: ErrorFormatter = { implicit formats => e =>
    Extraction.decompose(e.toList)
  }

  case class ValidationSpec[T, S <: JValue](
    validator: Validator[JValue, S]
  )(implicit ef: ErrorFormatter)

  class ValidationSerializer[T: Manifest, S <: JValue](implicit
    spec: ValidationSpec[T, S],
    ct: ClassTag[S],
    ef: ErrorFormatter) extends Serializer[JV[T]] {

    def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), JV[T]] = {
      case (t, ct(value)) if t.clazz == classOf[JV[T]] =>
        spec.validator(value).map(_.extract[T])
    }

    def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
      case Success(value: T) => Extraction.decompose(value)
      case Failure(Errors(e)) => ef(format)(e)
    }
  }
}
