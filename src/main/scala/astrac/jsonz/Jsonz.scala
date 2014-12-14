package astrac.jsonz

import org.json4s._
import org.json4s.native.JsonMethods._
import scala.reflect.ClassTag
import scala.util.matching.Regex
import scalaz._
import Scalaz._
import scala.collection.immutable.Iterable

case class ValidationError(error: String, path: List[String] = Nil)

object Jsonz {
  type JsonValidation[T] = ValidationNel[ValidationError, T]
  type Validator[T <: JValue] = JValue => JsonValidation[T]

  implicit def jvalueSemigroup[T <: JValue] = Semigroup.lastSemigroup[T]

  implicit def validatorMonoid[T <: JValue](implicit ct: ClassTag[T]) = new Monoid[Validator[T]] {
    def zero: Validator[T] = _ match {
      case ct(v) => v.asInstanceOf[T].successNel
      case v => ValidationError(s"Expected instance of ${ct.runtimeClass.getName}, got ${v.getClass.getName}").failureNel
    }
    def append(a: Validator[T], b: => Validator[T]): Validator[T] = v => { a(v) +++ b(v) }
  }

  implicit class StringValidatorOps(v: Validator[JString]) {
    def shouldMatch(r: Regex): Validator[JString] = v |+| {
      case jv @ JString(s) if s.matches(r.toString) => jv.successNel[ValidationError]
      case JString(s) => ValidationError(s"The string $s doesn't match the pattern ${r.toString}").failureNel[JString]
    }
  }

  implicit class ObjectValidatorOps(v: Validator[JObject]) {
    private def validatorForProp[T <: JValue](prop: String, validator: Validator[T]): Validator[JObject] = { jv =>
      validator(jv \ prop).bimap(
        _.map(err => err.copy(path = prop :: err.path)),
        _ => jv.asInstanceOf[JObject]) // TODO: Remove ugly asInstanceOf
    }

    def mustHave[T](props: Map[String, Validator[_ <: JValue]]): Validator[JObject] =
      props.foldLeft(v) { (v, prop) =>
        v |+| validatorForProp(prop._1, prop._2)
      }

    def mustHave[T](props: (String, Validator[_ <: JValue])*): Validator[JObject] = mustHave[T](props.toMap)
  }

  def validated[T <: JValue: ClassTag] = Monoid[Validator[T]].zero

  def optional[T <: JValue: ClassTag](v: Validator[T]): Validator[JValue] = { jv =>
    jv match {
      case JNothing | JNull => jv.successNel[ValidationError]
      case _ => v(jv)
    }
  }
}
