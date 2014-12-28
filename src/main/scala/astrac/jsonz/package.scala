package astrac

import scalaz._
import Scalaz._

package object jsonz {
  type JV[T] = ValidationNel[ValidationError, T]
  type Validator[S, T] = S => JV[T]
  type Rule[T] = Validator[T, T]
  type ErrorList = NonEmptyList[ValidationError]
}
