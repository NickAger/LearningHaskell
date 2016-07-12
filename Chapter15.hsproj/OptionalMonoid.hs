module OptionalMonoid (
    Optional (Nada, Only)
  ) where

import Data.Monoid 
  
-- Write the Monoid instance for our Maybe type renamed to Optional
-- Expected output:
-- Only (Sum 1) `mappend` Only (Sum 1) == Only (Sum {getSum = 2})
-- Only (Product 4) `mappend` Only (Product 2) == Only (Product {getProduct = 8})-- Only (Sum 1) `mappend` Nada == Only (Sum {getSum = 1})
-- Only [1] `mappend` Nada == Only [1]
-- Nada `mappend` Only (Sum 1) == Only (Sum {getSum = 1})

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)
  
instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend = optionalMappend
  
optionalMappend :: (Monoid a) => Optional a -> Optional a -> Optional a
optionalMappend Nada o = o
optionalMappend o Nada = o
optionalMappend (Only o1) (Only o2) = Only (o1 <> o2)