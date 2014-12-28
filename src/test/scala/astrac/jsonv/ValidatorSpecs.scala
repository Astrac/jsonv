package astrac.jsonv

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.json4s._
import org.json4s.native.JsonMethods._
import scalaz._
import Scalaz._

class ValidatorSpecs extends FlatSpec with Matchers {

  import JsonValidator._
  import Utils._

  "The DSL" should "create a validator for a one-property json object" in {
    val json = parse("""{ "foo": 1 }""")

    val validator = valid[JObject] <~ prop(_("foo")[JInt])

    validator(json) should equal(Success(json))
  }

  it should "validate a complex json object" in {
    val json = parse("""{ "foo": 1, "bar": { "baz": true, "qux": [ 1, 2, 3 ] }, "quz": "blag", "dub": 1.2 }""")

    val validator = valid[JObject] <~ props(
      _("foo")[JInt],
      _("bar")[JObject] <~ props(
        _("baz")[JBool],
        _("qux")[JArray]
      ),
      _("quz")[JString],
      _("dub")[JDouble]
    )

    validator(json) should equal(Success(json))
  }

  it should "collect errors and keep track of where they happened" in {
    val json = parse("""{ "foo": 1, "bar": false, "baz": { "qux": false } }""")

    val validator = valid[JObject] <~ props(
      _("foo")[JString],
      _("bar")[JString],
      _("baz")[JObject] <~ props(
        _("qux")[JInt]
      )
    )

    getPropertyErrors(validator(json)) should contain theSameElementsAs ("foo" :: "bar" :: "baz/qux" :: Nil)
  }

  it should "accept missing/null properties in an object if they are optional and validate them otherwise" in {
    val jsonMissing = parse("""{ "foo": 1, "bar": { "baz": true, "qux": [ 1, 2, 3 ] }, "quz": "blag", "dub": 1.2 }""")
    val jsonNull = parse("""{ "foo": 1, "bar": { "baz": true, "qux": [ 1, 2, 3 ] }, "quz": "blag", "dub": 1.2, "puk": null }""")
    val jsonOk = parse("""{ "foo": 1, "bar": { "baz": true, "qux": [ 1, 2, 3 ] }, "quz": "blag", "dub": 1.2, "puk": "ok" }""")

    val validator = valid[JObject] <~ props(
      _("foo")[JInt],
      _("bar")[JObject] <~ props(
        _("baz")[JBool],
        _("qux")[JArray]
      ),
      _("quz")[JString],
      _("dub")[JDouble],
      _("puk")[JString].?
    )

    validator(jsonMissing) should equal(Success(jsonMissing))
    validator(jsonNull) should equal(Success(jsonNull))
    validator(jsonOk) should equal(Success(jsonOk))
  }

  it should "provide errors if an optional property is provided but not valid" in {
    val json = parse("""{ "foo": 1, "bar": { "baz": true, "qux": [ 1, 2, 3 ] }, "quz": "blag", "dub": false }""")

    val validator = valid[JObject] <~ props(
      _("foo")[JInt],
      _("bar")[JObject] <~ props(
        _("baz")[JBool],
        _("qux")[JArray]
      ),
      _("quz")[JString],
      _("dub")[JDouble].?
    )

    getPropertyErrors(validator(json)) should contain theSameElementsAs ("dub" :: Nil)
  }

  it should "support boolean predicates" in {
    val json = parse("""{ "foo": 1, "bar": { "baz": true, "qux": [ 1, 2, 3 ] }, "quz": "blag", "dub": false }""")

    def failing(fail: Boolean) = valid[JObject] <~ props(
      _("foo")[JInt],
      _("bar")[JObject] <~ props(
        _("baz")[JBool],
        _("qux")[JArray]
      ),
      _("quz")[JString] <~ predicate(_ => !fail, "")
    )

    failing(false)(json) should equal(Success(json))
    getPropertyErrors(failing(true)(json)) should contain theSameElementsAs ("quz" :: Nil)
  }

  it should "support array isEmpty and isNotEmpty predicate" in {
    val emptyJson = parse("""{ "foo": 1, "bar": { "baz": true, "qux": [ ] }, "quz": "blag", "dub": false }""")
    val nonEmptyJson = parse("""{ "foo": 1, "bar": { "baz": true, "qux": [ 1 ] }, "quz": "blag", "dub": false }""")

    def validator(empty: Boolean) = valid[JObject] <~ props(
      _("foo")[JInt],
      _("bar")[JObject] <~ props(
        _("baz")[JBool],
        _("qux")[JArray] <~ (if (empty) isEmpty else isNotEmpty)
      )
    )

    validator(true)(emptyJson) should equal(Success(emptyJson))
    validator(false)(nonEmptyJson) should equal(Success(nonEmptyJson))
    getPropertyErrors(validator(false)(emptyJson)) should contain theSameElementsAs ("bar/qux" :: Nil)
    getPropertyErrors(validator(true)(nonEmptyJson)) should contain theSameElementsAs ("bar/qux" :: Nil)
  }
}
