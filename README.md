# Association lists

An *association list* is a simple representation of a key-value mapping as a list of key-value pairs. The `assoc-list` package provides functions for working with these kinds of data structures.

## Use case

We usually prefer to use [`Map`](https://hackage.haskell.org/package/containers) or [`HashMap`](https://hackage.haskell.org/package/unordered-containers) to represent finite mappings, because they have better performance characteristics and enforce the often-desirable requirement that each key be mapped to at most one value. But some use cases call for preserving the list-like aspect of data that has the semantics of a mapping.

For example, take the list of [header fields in an HTTP message](https://tools.ietf.org/html/rfc7230#section-3.2). Each header field is a key-value pair (the key is called the *field name*). Some field names may appear at most once among a message's header fields; others may appear repeatedly. The ordering of the header fields is significant only when a field name appears repeatedly.

What data structure should we use to represent a collection of header fields? Since we often need to perform lookups by field name, we might consider `Map String [String]` (where the field names have been normalized to all upper or lower case, since HTTP field names are case-insensitive). If we are only *consuming* HTTP messages, this might be suitable. But what if we are writing a proxy server that consumes a request, makes some modifications to the header fields, then forwards the modified message to another server? We would have to convert the client's list of header fields to a [`Map`](https://hackage.haskell.org/package/containers), perform the manipulations on the `Map`, and then convert back to a list. In the process, we lose the original ordering of the lists, as well as the capitalization of the field names. This does not affect the semantics of the message, but may nevertheless be an undesirable side effect. If we do not want this to happen, then we can keep the data in list format. But we need `Map`-like operations over it, which is where the `assoc-list` library comes in.

## Packages

This project consists of two packages whose contents are nearly the same:

* [`assoc-list`](http://hackage.haskell.org/package/assoc-list)
  * Defines an association list as `[(a, b)]`
  * Module names take the form `Data.AssocList.List.[___]`
* [`assoc-listlike`](http://hackage.haskell.org/package/assoc-listlike)
  * Defines an association list more generally as [`ListLike`](https://hackage.haskell.org/package/ListLike)` l (a, b) => l`
  * A concrete type for `AssocList a b` can be something like [`Seq`](https://hackage.haskell.org/package/containers)` (a, b)`
  * Module names take the form `Data.AssocList.ListLike.[___]`

For example, compare the following two type signatures for `mapFirst`:

* `Data.AssocList.List.Eq.mapFirst :: Eq a => a -> (b -> b) -> [(a, b)] -> [(a, b)]`
* `Data.AssocList.ListLike.Eq.mapFirst :: (ListLike l (a, b), Eq a) => a -> (b -> b) -> l -> l`

## Modules

* `Data.AssocList.[___].Concept`
  * Introduces foundational concepts
  * For example, the definition of the `AssocList` type
* `Data.AssocList.[___].Eq`
  * Functions that involve `Eq` constraints on the keys
  * For example, `lookupFirst :: Eq a => a -> AssocList a b -> Maybe b`
* `Data.AssocList.[___].Equivalence`
  * Most of the same functions as the `Eq` module, but with an `Equivalence` parameter instead of an `Eq` constraint
  * For example, `lookupFirst :: Equivalence a -> a -> AssocList a b -> Maybe b`
  * An example use case for this module might be the list of [header fields in an HTTP message](https://tools.ietf.org/html/rfc7230#section-3.2), which is an association list where the keys are case-insensitive
* `Data.AssocList.[___].Predicate`
  * Most of the same functions as the `Eq` module, but specifying keys using a `Predicate` rather than a particular key
  * For example, `lookupFirst :: Predicate a -> AssocList a b -> Maybe b`
* `Data.AssocList.[___].Ord`
  * Functions that involve `Ord` constraints on the keys
  * For example, `sortKeys :: Ord a => AssocList a b -> AssocList a b`
* `Data.AssocList.[___].Comparison`
  * The same functions as the `Ord` module, but with a `Comparison` parameter instead of an `Ord` constraint
  * For example, `sortKeys :: Comparison a -> AssocList a b -> AssocList a b`

## Related libraries

* The `base` package has some very limited support for association lists:
  * [`Data.List.lookup`](https://hackage.haskell.org/package/base-4.11.1.0/docs/Data-List.html#v:lookup)` :: Eq a => a -> [(a, b)] -> Maybe b`.
* The `hxt` (Haskell XML Toolbox) package defines a handful of functions in its [`Data.AssocList`](https://hackage.haskell.org/package/hxt-9.3.1.16/docs/Data-AssocList.html) module.
