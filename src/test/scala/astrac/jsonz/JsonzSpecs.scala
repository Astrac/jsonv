package astrac.jsonz

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.json4s._
import org.json4s.native.JsonMethods._
import scalaz._
import Scalaz._

class JsonzSpecs extends FlatSpec with Matchers {

  import Jsonz._

  def getPropertyErrors(v: ValidationNel[ValidationError, _]) = v.fold(_.toList, _ => Nil).map { case ValidationError(err, path) => path.mkString("/") }

  "The Jsonz DSL" should "validate a json object" in {
    val json = parse("""{ "foo": 1, "bar": { "baz": true, "qux": [ 1, 2, 3 ] }, "quz": "blag", "dub": 1.2 }""")

    val simpleValidator = isA[JObject] withProperties (
      "foo" -> isA[JInt],
      "bar" -> (isA[JObject] withProperties (
        "baz" -> isA[JBool],
        "qux" -> isA[JArray]
      )),
      "quz" -> isA[JString],
      "dub" -> isA[JDouble]
    )

    simpleValidator(json) should equal(Success(json))
  }

  it should "collect errors and keep track of where they happened" in {
    val json = parse("""{ "foo": 1, "bar": false, "baz": { "qux": false } }""")

    val simpleValidator = isA[JObject] withProperties (
      "foo" -> isA[JString],
      "bar" -> isA[JString],
      "baz" -> (isA[JObject] withProperties (
        "qux" -> isA[JInt]
      ))
    )

    getPropertyErrors(simpleValidator(json)) should contain theSameElementsAs("foo" :: "bar" :: "baz/qux" :: Nil)
  }
}
