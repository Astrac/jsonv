package astrac.jsonv

import org.json4s._
import org.json4s.native.JsonMethods._
import scala.reflect.ClassTag
import scala.util.matching.Regex
import scalaz._
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._

case class ValidationError(error: String, path: List[String] = Nil)

trait ValidatorOpsBase[S, T] {
  val v: Validator[S, T]

  implicit val semigroup = Semigroup.lastSemigroup[T]

  def isA[TT <: T](implicit ct: ClassTag[TT]): Validator[S, TT] = { s: S =>
    v(s).flatMap {
      _ match {
        case ct(tt) => tt.successNel[ValidationError]
        case invalid => ValidationError(s"Expected instance of ${ct.runtimeClass}, got ${invalid.getClass.getName}").failureNel[TT]
      }
    }
  }

  def <~(rules: Rule[T]*): Validator[S, T] = { s: S =>
    val vt = v(s)

    vt.fold(_ => vt, _ => vt +++ vt.flatMap { t: T =>
      rules.map(_(t)).foldLeft(t.successNel[ValidationError])(_ +++ _)
    })
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
    else ValidationError(s"The value ${js.s} does not match /${r.toString}/").failureNel[JString]
  }

  case class PropertyBuilder(val name: String) {
    def apply[T <: JValue: ClassTag]: (String, Validator[JValue, T]) = (name, aValid[JValue].isA[T])
  }

  type PropertySpec = (String => PropertyBuilder) => (String, Validator[JValue, _])

  def prop(p: PropertySpec): Rule[JObject] = { jo =>
    val (name, v) = p(PropertyBuilder)

    val x = v(jo \ name)
    x.fold(
      _.map(e => e.copy(path = name :: e.path)).failure[JObject],
      _ => jo.successNel[ValidationError])
  }

  def props(ps: PropertySpec*): Rule[JObject] = { jo =>
    implicit val semigroup = Semigroup.lastSemigroup[JObject]
    ps.map(prop).foldLeft(aValid[JObject](jo))(_ +++ _(jo))
  }


  def path[T <: JValue : ClassTag](s1: String, segments: String*)(v: Rule[T]): Rule[JObject] = {
    def pathImpl[T <: JValue : ClassTag](segments: List[String])(v: Rule[T]): Rule[JObject] = {
      segments match {
        case Nil =>
          sys.error("Unexpected empty list in path function")
        case s :: Nil =>
          prop(_(s)[T] <~ v)
        case s :: ss =>
          prop(_(s)[JObject] <~ pathImpl(ss)(v))
      }
    }

    pathImpl(s1 :: segments.toList)(v)
  }

  def predicate[T <: JValue](p: T => Boolean, msg: String): Rule[T] = { t =>
    if (p(t)) t.successNel[ValidationError]
    else ValidationError(msg).failureNel[T]
  }

  def isEmpty[T <: JArray] = predicate[JArray](_.arr.isEmpty, "The array should be empty")
  def isNotEmpty[T <: JArray] = predicate[JArray](!_.arr.isEmpty, "The array should not be empty")

  def valid[T <: JValue : ClassTag]: Validator[JValue, T] = aValid[JValue].isA[T]
}
