From comments section in [Type-classes are nothing like interfaces](http://blog.tmorris.net/posts/type-classes-are-nothing-like-interfaces/):

You do realize that type classes are just syntactic sugar for data types right?

When you write:

```haskell
class Comparator a where
    compare :: a -> a -> Int
```

It is actually desugared to:

```haskell
data Comparator a = MakeComparator { compare :: a -> a -> Int }
```

Similarly, for instances:

```haskell
instance Comparator Int where
    compare a b
        | a == b = 0
        | a <= b = (-1)
        | otherwise = 1
```

This is desugared to:

```haskell
dataComparatorInt :: Comparator Int
dataComparatorInt = MakeComparator compare
    where compare a b
        | a == b = 0
        | a <= b = (-1)
        | otherwise = 1
```

