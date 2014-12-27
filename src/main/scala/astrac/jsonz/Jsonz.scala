package astrac.jsonz

import org.json4s._
import org.json4s.native.JsonMethods._
import scala.reflect.ClassTag
import scala.util.matching.Regex
import scalaz._
import Scalaz._
import scalaz.Validation.FlatMap._
import scala.collection.immutable.Iterable

case class ValidationError(error: String, path: List[String] = Nil)

object Types {
  type JV[T] = ValidationNel[ValidationError, T]
  type Validator[S, T] = S => JV[T]
  type Rule[T] = Validator[T, T]
}

import Types._

trait ValidatorOpsBase[S, T] {
  val v: Validator[S, T]

  implicit val semigroup = Semigroup.lastSemigroup[T]

  def isA[TT <: T](implicit ct: ClassTag[TT]): Validator[S, TT] = { s: S =>
    v(s).flatMap {
      _ match {
        case ct(tt) => tt.successNel[ValidationError]
        case _ => ValidationError("").failureNel[TT]
      }
    }
  }

  def <~(rules: Rule[T]*): Validator[S, T] = { s: S =>
    val vt = v(s)
    vt +++ vt.flatMap { t: T =>
      rules.map(_(t)).foldLeft(t.successNel[ValidationError])(_ +++ _)
    }
  }
}

trait LowPriorityValidatorImplicits {
  implicit class RuleOps[T](val v: Rule[T]) extends ValidatorOpsBase[T, T]
}

trait ValidatorDsl extends LowPriorityValidatorImplicits {

  implicit def validatorMonad[S] = new Monad[({ type L[T] = Validator[S, T] })#L] {
    def point[A](a: => A): Validator[S, A] = { v: S => a.successNel[ValidationError] }
    def bind[A, B](fa: Validator[S, A])(f: A => Validator[S, B]): Validator[S, B] = { v: S =>
      fa(v).fold(_.failure[B], a => f(a)(v))
    }
  }

  def aValid[S]: Rule[S] = { v => v.successNel[ValidationError] }

  implicit class ValidatorOps[S, T](val v: Validator[S, T]) extends ValidatorOpsBase[S, T]
}

object JsonValidator extends ValidatorDsl {
  implicit class JValueValidatorOps[T <: JValue](v: Validator[JValue, T]) {
    def ? : Validator[JValue, T] = { t: JValue =>
      t match {
        case tt @ (JNull | JNothing) => tt.asInstanceOf[T].successNel[ValidationError]
        case _ => v(t)
      }
    }
  }

  implicit class PropertyValidatorOps[T <: JValue](pv: (String, Validator[JValue, T])) {
    val (name, v) = pv
    def <~(rules: Rule[T]*): (String, Validator[JValue, T]) = (name, ValidatorOps(v).<~(rules: _*))
    def ? : (String, Validator[JValue, T]) = (name, v.?)
  }

  def matchRegex(r: Regex): Rule[JString] = { js =>
    if (js.s.matches(r.toString)) js.successNel[ValidationError]
    else ValidationError("???").failureNel[JString]
  }

  case class PropertyBuilder(val name: String) {
    def apply[T <: JValue: ClassTag]: (String, Validator[JValue, T]) = (name, aValid[JValue].isA[T])
  }

  type PropertySpec = (String => PropertyBuilder) => (String, Validator[JValue, _])

  def prop(p: PropertySpec): Rule[JObject] = { jo =>
    val (name, v) = p(PropertyBuilder)

    v(jo \ name).fold(
      _.map(e => e.copy(path = name :: e.path)).failure[JObject],
      _ => jo.successNel[ValidationError])
  }

  def props(ps: PropertySpec*): Rule[JObject] = { jo =>
    implicit val semigroup = Semigroup.lastSemigroup[JObject]
    ps.map(prop).foldLeft(aValid[JObject](jo))(_ +++ _(jo))
  }

  def predicate[T <: JValue](p: T => Boolean, msg: String): Rule[T] = { t =>
    if (p(t)) t.successNel[ValidationError]
    else ValidationError(msg).failureNel[T]
  }

  def isEmpty[T <: JArray] = predicate[JArray](_.arr.isEmpty, "The array should be empty")
  def isNotEmpty[T <: JArray] = predicate[JArray](!_.arr.isEmpty, "The array should not be empty")

  def valid[T <: JValue : ClassTag]: Validator[JValue, T] = aValid[JValue].isA[T]
}
