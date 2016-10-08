See also "Phantom types" in [Four months with Haskell](https://lexi-lambda.github.io/blog/2016/06/12/four-months-with-haskell/)

---

From: https://wiki.haskell.org/Phantom_type
```haskell
class Sanitise a where
  sanitise :: FormData a -> FormData Validated
 
-- do nothing to data that is already validated
instance Sanitise Validated where
  sanitise = id
 
-- sanitise untrusted data
instance Sanitise Unvalidated where
  sanitise (FormData str) = FormData (filter isAlpha str)
```

---

Phantom types in [OpenCV](https://github.com/LumiGuide/haskell-opencv) haskell bindings.
