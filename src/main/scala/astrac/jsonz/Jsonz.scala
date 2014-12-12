package astrac.jsonz

import org.json4s._
import org.json4s.native.JsonMethods._
import scalaz._
import Scalaz._

sealed trait Validator[T <: JValue] {
  def apply(v: JValue): ValidationNel[ValidationError, T]
}
sealed trait ValidationError
sealed trait PropertyValidator[T <: JValue] extends Validator[T] {
  def name: String
}

object Jsonz {
  def validatorFor[T <: JValue]: Validator[T] = ???

  implicit class ObjectValidatorOps[V <: Validator[JObject]](ov: V) {
    def withProperties(properties: PropertyValidator[_]*): V = ???
  }

  implicit class StringOps(str: String) {
    def isA[T <: JValue] : PropertyValidator[T] = ???
  }

  implicit class PropertyValidatorOps[T <: JValue](pv: PropertyValidator[T]) {
    def required: PropertyValidator[T] = ???
    def optional: PropertyValidator[T] = ???
  }
}

object Example {
  import Jsonz._

  /**
    * { "foo": { "bar": [1, 2, 3], "baz": 10 }, "qux": 1.2, "quz": true, "ping": "pong" }
    */
  val fooValidator: Validator[JObject] = validatorFor[JObject] withProperties(
    "qux".isA[JDecimal] required,
    "quz".isA[JBool] optional,
    "ping".isA[JString] optional /*matchingRegex("pong".r)*/,
    "foo".isA[JObject] withProperties(
      "bar".isA[JArray],
      "baz".isA[JInt]
    )
  )

  val x: JValue = ???

  val validated: ValidationNel[ValidationError, JObject] = fooValidator(x)
}
