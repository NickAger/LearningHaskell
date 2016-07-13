From Chapter 14, Monoid, Semigroup

## NonEmpty, a useful datatype
One really useful datatype that can’t have a Monoid instance but does have a Semigroup instance is the NonEmpty list type. It is a list datatype that can never be an empty list:
```haskell
data NonEmpty a = a :| [a] deriving (Eq, Ord, Show)
```

Because NonEmpty is a product of two arguments, we could’ve also written it as:
```haskell
newtype NonEmpty a = NonEmpty (a, [a]) deriving (Eq, Ord, Show)
```
