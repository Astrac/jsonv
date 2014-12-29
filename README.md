# Jsonv

A library that provides the following:

* A simple and extensible DSL to create JSON validators based on the Json4s AST and on scalaz `Validation`
* A Json4s serializer that can serialize and de-serialize scalaz `Validation` instances

## Quick example

From tests:

```
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
```

For examples of usage please refer to tests.

## TODO

Things that I would like to add:

* More validation predicates
* Support for building the base validator from a case class (if possible)
* More documentation