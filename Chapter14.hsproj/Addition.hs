module Addition where

import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

recursiveSum :: (Eq a, Num a) => a -> a
recursiveSum 1 = 1
recursiveSum n = n + recursiveSum (n - 1)

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 2, 2, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering 
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)
  
genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)


genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]
  
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]
  
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing), (3, return (Just a))]
  
prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 0 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

main :: IO ()
main = return ()
--main = hspec $ do
--  describe "Addition" $ do
--    it "15 divided by 3 is 5" $ do
--      dividedBy 15 3 `shouldBe` (5, 0)
--    it "22 divided by 5 is 4 reminder 2" $ do
--      dividedBy 22 5 `shouldBe` (4, 2)
--    it "1 recursively summed is 1" $ do
--      recursiveSum 1 `shouldBe` 1
--    it "2 recursively summed is 3" $ do
--      recursiveSum 2 `shouldBe` 3
--    it "4 recursively summed is 10" $ do
--      recursiveSum 4 `shouldBe` 10
--    it "x + 1 is always greater than x" $ do
--      property $ \x -> x + 1 > (x :: Int)
