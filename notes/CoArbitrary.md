From Chapter 14

It turns out, as long as your datatype has a Generic instance derived, you can get these instances [CoArbitary instances] for free. 

The following should work just fine:

```haskell
{-# LANGUAGE DeriveGeneric #-}
module CoArbitrary where 

import GHC.Generics
import Test.QuickCheck

data Bool' =
    True'
| False'
deriving (Generic)

instance CoArbitrary Bool'
```

need to ensure you `import GHC.Generics` otherwise you'll endup with `Not in scope: type constructor or class ‘Generic’`
