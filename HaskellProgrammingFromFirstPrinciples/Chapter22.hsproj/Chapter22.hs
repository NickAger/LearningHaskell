import Control.Applicative

hurr :: Integer -> Integer
hurr = (*2)

durr :: Integer -> Integer
durr = (+10)

m :: Integer -> Integer
m = hurr . durr

m' :: Integer -> Integer
m' = fmap hurr durr

m2 :: Integer -> Integer
m2 = (+) <$> hurr <*> durr

m3 :: Integer -> Integer
m3 = liftA2 (+) hurr durr