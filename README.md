# Association lists

An *association list* is a simple representation of a key-value mapping as a list of key-value pairs. The `assoc-list` package provides functions for working with these kinds of data structures.

## Packages in this project:

* `assoc-list`
* `assoc-listlike`

## Related libraries

* The `base` package has some very limited support for association lists:
  * [`Data.List.lookup`](https://hackage.haskell.org/package/base-4.11.1.0/docs/Data-List.html#v:lookup)` :: Eq a => a -> [(a, b)] -> Maybe b`.
* The `hxt` (Haskell XML Toolbox) package defines a handful of functions in its [`Data.AssocList`](https://hackage.haskell.org/package/hxt-9.3.1.16/docs/Data-AssocList.html) module.
