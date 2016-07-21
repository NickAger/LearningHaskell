From Chapter 21:

Explaining the double `.` in:

```haskell
(sequence .) . fmap
```

The weird looking composition, which you’ve possibly also seen in the form of `(join .) . fmap` is because `fmap` takes 
two (not one) arguments, so the expressions aren’t proper unless we compose twice to await a second argument for `fmap`
to get applied to.

```haskell
(sequence .) . fmap ≣ \ f xs -> sequence (fmap f xs)
```

whereas:

```haskell
sequence . fmap ≣ \ f -> sequence (fmap f)
```

