package astrac.jsonz

import org.json4s._
import org.json4s.native.JsonMethods._
import scala.reflect.ClassTag
import scalaz._
import Scalaz._
import scala.collection.immutable.Iterable

case class ValidationError(error: String, path: List[String] = Nil)

sealed trait Validator[T <: JValue] {
  def apply(v: JValue): ValidationNel[ValidationError, T]
}

sealed trait ValidatorProto[T <: JValue] {
  def validator: Validator[T]
}

case class ObjectValidator(properties: Map[String, Validator[_ <: JValue]]) extends Validator[JObject] {
  private def prefixErrors(propName: String, errors: NonEmptyList[ValidationError]): List[ValidationError] =
    errors.map(e => e.copy(path = propName :: e.path)).toList

  private def validateProps(obj: JObject): ValidationNel[ValidationError, JObject] =
    properties.flatMap {
      case (name, v) => v(obj \ name).fold(errors => prefixErrors(name, errors), _ => Nil)
    }.toList.toNel.fold(obj.successNel[ValidationError])(_.failure[JObject])

  def apply(v: JValue): ValidationNel[ValidationError, JObject] =
    v.isInstanceOf[JObject]
      .option(v.asInstanceOf[JObject])
      .fold(ValidationError("Expected object, got " + v).failureNel[JObject])(validateProps)
}

class DummyValidator[T <: JValue](implicit ct: ClassTag[T]) extends Validator[T] {
  def apply(v: JValue): ValidationNel[ValidationError, T] = v match {
    case ct(value) => value.successNel[ValidationError]
    case _ => ValidationError(s"Expected ${ct.runtimeClass.getName}, got " + v.getClass.getName).failureNel[T]
  }
}

object Jsonz {
  implicit val JObjectValidatorProto: ValidatorProto[JObject] = new ValidatorProto[JObject] { def validator = ObjectValidator(Map.empty) }
  implicit val JIntValidatorProto: ValidatorProto[JInt] = new ValidatorProto[JInt] { def validator = new DummyValidator }
  implicit val JBoolValidatorProto: ValidatorProto[JBool] = new ValidatorProto[JBool] { def validator = new DummyValidator }
  implicit val JStringValidatorProto: ValidatorProto[JString] = new ValidatorProto[JString] { def validator = new DummyValidator }
  implicit val JArrayValidatorProto: ValidatorProto[JArray] = new ValidatorProto[JArray] { def validator = new DummyValidator }
  implicit val JDoubleValidatorProto: ValidatorProto[JDouble] = new ValidatorProto[JDouble] { def validator = new DummyValidator }

  def isA[T <: JValue](implicit p: ValidatorProto[T]): Validator[T] = p.validator

  implicit class ObjectValidatorOps(validator: Validator[JObject]) {
    def withProperties(props: (String, Validator[_ <: JValue])*): Validator[JObject] = withProperties(props.toMap)
    def withProperties(props: Map[String, Validator[_ <: JValue]]): Validator[JObject] = ObjectValidator(props)
  }
}
