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

    val validator = validated[JObject] mustHave (
      "foo" -> validated[JInt],
      "bar" -> (validated[JObject] mustHave (
        "baz" -> validated[JBool],
        "qux" -> validated[JArray]
      )),
      "quz" -> validated[JString],
      "dub" -> validated[JDouble]
    )

    validator(json) should equal(Success(json))
  }

  it should "collect errors and keep track of where they happened" in {
    val json = parse("""{ "foo": 1, "bar": false, "baz": { "qux": false } }""")

    val validator = validated[JObject] mustHave (
      "foo" -> validated[JString],
      "bar" -> validated[JString],
      "baz" -> (validated[JObject] mustHave (
        "qux" -> validated[JInt]
      ))
    )

    getPropertyErrors(validator(json)) should contain theSameElementsAs("foo" :: "bar" :: "baz/qux" :: Nil)
  }

  it should "accept missing/null properties in an object if they are optional" in {
    val jsonMissing = parse("""{ "foo": 1, "bar": { "baz": true, "qux": [ 1, 2, 3 ] }, "quz": "blag", "dub": 1.2 }""")
    val jsonNull = parse("""{ "foo": 1, "bar": { "baz": true, "qux": [ 1, 2, 3 ] }, "quz": "blag", "dub": 1.2, "puk": null }""")

    val validator = validated[JObject] mustHave (
      "foo" -> validated[JInt],
      "bar" -> (validated[JObject] mustHave (
        "baz" -> validated[JBool],
        "qux" -> validated[JArray]
      )),
      "quz" -> validated[JString],
      "dub" -> validated[JDouble],
      "puk" -> optional(validated[JString])
    )

    validator(jsonMissing) should equal(Success(jsonMissing))
    validator(jsonNull) should equal(Success(jsonNull))
  }

  it should "provide errors if an optional property is provided but not validated" in {
    val json = parse("""{ "foo": 1, "bar": { "baz": true, "qux": [ 1, 2, 3 ] }, "quz": "blag", "dub": false }""")

    val validator = validated[JObject] mustHave (
      "foo" -> validated[JInt],
      "bar" -> (validated[JObject] mustHave (
        "baz" -> validated[JBool],
        "qux" -> validated[JArray]
      )),
      "quz" -> validated[JString],
      "dub" -> optional(validated[JDouble])
    )

    getPropertyErrors(validator(json)) should contain theSameElementsAs("dub" :: Nil)
  }
}
