package astrac.jsonz

import org.json4s._
import org.json4s.native.JsonMethods._
import scalaz._

sealed trait Validator[T <: JValue]

object Jsonz {
  sealed trait PropertyOpt
  sealed trait Required extends PropertyOpt
  sealed trait Optional extends PropertyOpt

  case class Properties(validators: Set[PropertyValidator[_, _]])
  object Properties { def empty = Properties(Set.empty) }

  case class PropertyValidator[T <: JValue, Opt <: PropertyOpt](name: String, rules: List[Rule[T]]) extends Validator[T]

  case class ObjectValidator(properties: Properties) extends Validator[JObject]

  case class PropertiesBuilder(validators: Set[PropertyValidator[_, _]])
  object PropertiesBuilder { def empty = PropertiesBuilder(Set.empty) }

  case class ValidationFailure(property: String, value: JValue, message: String)

  type Rule[T] = T => ValidationNel[ValidationFailure, T]

  case class PropertyValidatorBuilder[T <: JValue](name: String, rules: List[Rule[T]])

  implicit class PropertiesBuilderOps(pb: Properties) {
    def required[T <: JValue](name: String) = pb.copy(validators = pb.validators + PropertyValidator[T, Required](name, Nil))
    def optional[T <: JValue](name: String) = pb.copy(validators = pb.validators + PropertyValidator[T, Optional](name, Nil))
  }

  implicit class ObjectValidatorOps(ov: ObjectValidator) {
    def withProperties(propSpec: Properties => Properties) = ov.copy(properties = propSpec(Properties.empty))
  }

  def validatorFor[T <: JValue] = ObjectValidator(Properties(Set.empty))
}

object Example {
  import Jsonz._

  /**
    * { "foo": { "bar": [1, 2, 3], "baz": 10 }, "qux": 1.2, "quz": true, "ping": "pong" }
    */
  validatorFor[JObject] withProperties {
    _.required[JDecimal]("qux")
     .required[JBool]("quz")
     .optional[JObject]("foo") withProperties {
       .required[JArray]("bar")
       .optional[JInt]("baz") }
     .required[JString]("ping") matchingRegex("pong".r)
  }
}
