package astrac.jsonz

import org.json4s.native.Serialization
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.json4s._
import org.json4s.native.JsonMethods._
import scalaz._
import Scalaz._
import Json4s._
import JsonValidator._
import Utils._

class Json4sSerializerSpecs extends FlatSpec with Matchers {

  implicit val ef: ErrorFormatter = defaultErrorFormatter

  implicit val validationSpec = ValidationSpec[Foobar, JObject](valid[JObject] <~ props(
    _("foo")[JInt],
    _("bar")[JBool],
    _("baz")[JObject] <~ props(
      _("qux")[JInt]
    )
  ))

  implicit val formats = DefaultFormats + (new ValidationSerializer[Foobar, JObject])

  "The validated json4s serializer" should "support deserializing valid data to a Validation instance" in {
    val json = parse("""{ "foo": 1, "bar": false, "baz": { "qux": 1 } }""")
    json.extract[JV[Foobar]] should equal(Success(Foobar(1, false, Baz(1))))
  }

  it should "support deserializing invalid data to a Validation instance" in {
    val json = parse("""{ "foo": 1, "bar": null, "baz": { "qux": false } }""")
    getPropertyErrors(json.extract[JV[Foobar]]) should contain theSameElementsAs ("bar" :: "baz/qux" :: Nil)
  }

  it should "support serializing a validation success to json" in {
    Extraction.decompose(Foobar(1, false, Baz(1)).successNel[ValidationError]) should equal(
      parse("""{ "foo": 1, "bar": false, "baz": { "qux": 1 } }"""))
  }

  it should "support serializing a validation failure to json" in {
    val errors = NonEmptyList(
      ValidationError("An error", "foo" :: Nil),
      ValidationError("Another error", "bar" :: "baz" :: Nil))

    Extraction.decompose(errors.failure[Foobar]) should equal(
      parse("""[
        {"error": "An error", "path": ["foo"]},
        {"error": "Another error", "path": ["bar", "baz"]}]"""))
  }
}
