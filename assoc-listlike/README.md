# assoc-listlike

An association list conceptually signifies a mapping, but is represented as a list (of key-value pairs).

This package defines an association list as a constraint synonym for a list-like collection of tuples, using the `ListLike` type class from the [`ListLike`](http://hackage.haskell.org/package/ListLike) package.

```haskell
type AssocList l a b = ListLike l (a, b)
```
