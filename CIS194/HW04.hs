{-# OPTIONS_GHC -Wall #-}
module HW04 where
import Data.Maybe (fromMaybe)

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [1, 0]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P a) (P b) =  removeTrailingZeros a == removeTrailingZeros b
        where
            removeTrailingZeros :: (Num a, Eq a) => [a] -> [a]
            removeTrailingZeros cs = foldl filterZeroIfEmptyAccumulator [] $ reverse cs
                where filterZeroIfEmptyAccumulator accumulator coefficent = if null accumulator && coefficent == 0 
                        then [] 
                        else coefficent:accumulator
 
-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show p = showEquation $ filter (not . null) (makeTerms 0 p) where
        showEquation :: [String] -> String
        showEquation [] = ""
        showEquation [t0] = t0
        showEquation (t:ts) = t ++ " + " ++ showEquation ts

        makeTerms :: (Num a, Eq a, Show a) => a -> Poly a -> [String] 
        makeTerms _ (P []) = []
        makeTerms ex (P (a:as)) = makeTerms (ex + 1) (P as) ++ [showTerm a ex]
            where 
                showTerm :: (Num c, Eq c, Show c, Num e, Eq e, Show e) => c -> e -> String
                showTerm 0 _ = ""
                showTerm c 0 = show c
                showTerm 1 1 = "x"
                showTerm c 1 = show c ++ "x"
                showTerm 1 e = "x^" ++ show e
                showTerm c e = show c ++ "x^" ++ show e


-- Exercise 4 -----------------------------------------

-- idea for mapping to Maybe taken from http://stackoverflow.com/questions/21349408/zip-with-default-value-instead-of-dropping-values
plus :: (Num a, Eq a) => Poly a -> Poly a -> Poly a
plus (P xs) (P ys) = P (sumLists xs ys)
    where
        sumLists :: (Num a, Eq a) => [a] -> [a] -> [a]
        sumLists xs' ys' = 
          map (\(x',y') -> fromMaybe 0 x' + fromMaybe 0 y') $ 
            takeWhile ((Nothing, Nothing) /=) $ 
            zip (map Just xs' ++ repeat Nothing) (map Just ys' ++ repeat Nothing)

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times = undefined

-- Exercise 6 -----------------------------------------

instance (Num a, Eq a) => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger = undefined
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance (Num a, Eq a) => Differentiable (Poly a) where
    deriv = undefined

