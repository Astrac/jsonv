# Jsonv

A library that provides the following:

* A simple and extensible DSL to create JSON validators based on the Json4s AST and on scalaz `Validation`
* A Json4s serializer that can serialize and de-serialize scalaz `Validation` instances

For examples of usage please refer to tests.

## TODO

Things that I would like to add:

* More validation predicates
* Support for building the base validator from a case class (if possible)
* More documentation